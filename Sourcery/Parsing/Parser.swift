//
//  Parser.swift
//  Sourcery
//
//  Created by Dmitriy on 1/4/19.
//  Copyright © 2019 Pixle. All rights reserved.
//

import Foundation
import SourceryRuntime
import SwiftTryCatch
import PathKit

struct Parser {
    enum Error: Swift.Error {
        case containsMergeConflictMarkers
    }

    typealias ParsingResult = (types: Types, inlineRanges: [(file: String, ranges: [String: NSRange])])

    let paths: Paths
    let forceParse: [String]
    let modules: [String]?
    let verboseLogging: Bool
    let cachesPath: (Path) -> Path?

    init(paths: Paths,
         forceParse: [String] = [],
         modules: [String]? = nil,
         verboseLogging: Bool,
         cachesPath: @escaping (Path) -> Path? = { _ in nil }) {
        self.paths = paths
        self.forceParse = forceParse
        self.modules = modules
        self.cachesPath = cachesPath
        self.verboseLogging = verboseLogging
    }

    func parse() throws -> ParsingResult {
        if let modules = modules {
            precondition(paths.include.count == modules.count, "There should be module for each file to parse")
        }

        Log.info("Scanning sources...")

        var inlineRanges = [(file: String, ranges: [String: NSRange])]()
        var allResults = [FileParserResult]()

        try paths.include.enumerated().forEach { index, from in
            let fileList = from.isDirectory ? try from.recursiveChildren() : [from]
            let sources = try fileList
                .filter { $0.isSwiftSourceFile }
                .filter {
                    let exclude = paths.exclude
                        .map { $0.isDirectory ? try? $0.recursiveChildren() : [$0] }
                        .compactMap({ $0 }).flatMap({ $0 })
                    return !exclude.contains($0)
                }
                .compactMap { (path: Path) -> (path: Path, contents: String)? in
                    do {
                        return (path: path, contents: try path.read(.utf8))
                    } catch {
                        Log.warning("Skipping file at \(path) as it does not exist")
                        return nil
                    }
                }
                .filter {
                    let result = Verifier.canParse(content: $0.contents,
                                                   path: $0.path,
                                                   forceParse: forceParse)
                    if result == .containsConflictMarkers {
                        throw Error.containsMergeConflictMarkers
                    }

                    return result == .approved
                }
                .map {
                    try FileParser(contents: $0.contents, path: $0.path, module: modules?[index])
            }

            let map = { try self.loadOrParse(parser: $0, cachesPath: self.cachesPath(from)) }
            let results = try sources.parallelMap(map) { _ in
                self.logProgress(sourcesCount: sources.count)
            }

            allResults.append(contentsOf: results)
        }

        let parserResult = allResults.reduce(FileParserResult(path: nil, module: nil, types: [], typealiases: [])) { acc, next in
            acc.typealiases += next.typealiases
            acc.types += next.types

            // swiftlint:disable:next force_unwrapping
            inlineRanges.append((next.path!, next.inlineRanges))
            return acc
        }

        //! All files have been scanned, time to join extensions with base class
        let types = Composer().uniqueTypes(parserResult)

        Log.info("Found \(types.count) types.")
        return (Types(types: types), inlineRanges)
    }

    private func logProgress(sourcesCount: Int) {
        guard verboseLogging else {
            return
        }

        var previousUpdate = 0
        var accumulator = 0
        let step = sourcesCount / 10 // every 10%

        guard accumulator > previousUpdate + step else {
            accumulator += 1
            return
        }

        previousUpdate = accumulator
        let percentage = accumulator * 100 / sourcesCount
        Log.info("Scanning sources... \(percentage)% (\(sourcesCount) files)")
    }

    private func loadOrParse(parser: FileParser, cachesPath: @autoclosure () -> Path?) throws -> FileParserResult {
        guard let pathString = parser.path else { fatalError("Unable to retrieve \(String(describing: parser.path))") }

        guard let cachesPath = cachesPath() else {
            return try parser.parse()
        }

        let path = Path(pathString)
        let artifacts = cachesPath + "\(pathString.hash).srf"

        guard artifacts.exists,
            let contentSha = parser.initialContents.sha256(),
            let unarchived = load(artifacts: artifacts.string, contentSha: contentSha) else {

                let result = try parser.parse()

                let data = NSKeyedArchiver.archivedData(withRootObject: result)
                do {
                    try artifacts.write(data)
                } catch {
                    fatalError("Unable to save artifacts for \(path) under \(artifacts), error: \(error)")
                }

                return result
        }

        return unarchived
    }

    private func load(artifacts: String, contentSha: String) -> FileParserResult? {
        var unarchivedResult: FileParserResult?
        SwiftTryCatch.try({
            if let unarchived = NSKeyedUnarchiver.unarchiveObject(withFile: artifacts) as? FileParserResult, unarchived.sourceryVersion == Sourcery.version, unarchived.contentSha == contentSha {
                unarchivedResult = unarchived
            }
        }, catch: { _ in
            Log.warning("Failed to unarchive \(artifacts) due to error, re-parsing")
        }, finallyBlock: {})

        return unarchivedResult
    }
}
