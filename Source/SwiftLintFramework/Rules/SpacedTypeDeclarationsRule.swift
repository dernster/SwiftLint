//
//  LineAfterDeclarationRule.swift
//  SwiftLint
//
//  Created by Diego Ernst on 1/31/17.
//  Copyright © 2017 Realm. All rights reserved.
//

import Foundation
import SourceKittenFramework

public struct SpacedTypeDeclarationsRule: ASTRule, OptInRule, ConfigurationProviderRule, CorrectableRule {
    public var configuration = SpacedTypeDelcarationsConfiguration()
    private var declarationTypes: [SwiftDeclarationKind] = [.class, .enum, .struct, .protocol, .extension]

    public static let description = RuleDescription(
        identifier: "spaced_type_declarations",
        name: "Spaced Type Declarations",
        description: "The start and end of a type declaration must be enclosed by two empty lines.",
        nonTriggeringExamples: [
            "class Example { }\n",
            "class Example {\n" +
            "\n" +
            "}\n" +
            "",
            " /* multiline\n" +
            "    comment */\n" +
            "enum Enum { // testing comments\n" +
            "\n" +
            "   case first\n" +
            "   case second\n" +
            "\n" +
            "}\n" +
            ""
        ],
        triggeringExamples: [
            "↓extension Example {\n" +
            "↓   func doSomething() { }\n" +
            "↓}\n" +
            "protocol SomeProtocol { }\n",
            "struct Struct {\n" +
            "\n" +
            "↓  let someConstante: Int?\n" +
            "}\n",
            "struct Struct {\n" +
            "\n" +
            "↓  let someConstante: Int?\n" +
            "↓}\n" +
            "extension Another { }\n"
        ],
        corrections: [
            "↓extension Example {\n" +
            "↓   func doSomething() { }\n" +
            "↓}\n" +
            "protocol SomeProtocol { }\n":
            "extension Example {\n\n" +
            "   func doSomething() { }\n\n" +
            "}\n\n" +
            "protocol SomeProtocol { }\n",
            "struct Struct {\n" +
            "\n" +
            "↓  let someConstante: Int?\n" +
            "}\n":
            "struct Struct {\n" +
            "\n" +
            "  let someConstante: Int?\n\n" +
            "}\n",
            "struct Struct {\n" +
            "\n" +
            "↓  let someConstante: Int?\n" +
            "↓}\n" +
            "extension Another { }\n":
            "struct Struct {\n" +
            "\n" +
            "  let someConstante: Int?\n\n" +
            "}\n\n" +
            "extension Another { }\n",
            "↓class Class {\n" +
            "   let array = [\n" +
            "       1,\n" +
            "       2\n" +
            "↓   ]\n" +
            "}\n":
            "class Class {\n\n" +
            "   let array = [\n" +
            "       1,\n" +
            "       2\n" +
            "   ]\n\n" +
            "}\n"
        ]
    )

    public init() {}

    public func validate(file: File, kind: SwiftDeclarationKind,
                         dictionary: [String: SourceKitRepresentable]) -> [StyleViolation] {
        guard
            declarationTypes.contains(kind),
            let bodyOffset = dictionary.bodyOffset,
            let bodyLength = dictionary.bodyLength,
            let (startLine, _) = file.contents.bridge().lineAndCharacter(forByteOffset: bodyOffset),
            let (endLine, _) = file.contents.bridge().lineAndCharacter(forByteOffset: bodyOffset + bodyLength),
            startLine != endLine
        else {
            return []
        }
        return findErrors(startLine: startLine, endLine: endLine, file: file)
    }

    private func findErrors(startLine: Int, endLine: Int, file: File) -> [StyleViolation] {
        var errorLines = [Int]()
        errorLines.append(contentsOf: checkOuterPadding(beginning: startLine - 1, end: endLine - 1, file: file))
        errorLines.append(contentsOf: checkInnerPadding(beginning: startLine - 1, end: endLine - 1, file: file))
        return errorLines.unique.map { StyleViolation(
                ruleDescription: type(of: self).description,
                severity: self.configuration.severityConfiguration.severity,
                location: Location(file: file.path, line: $0, character: 1)
            )
        }
    }

    private func checkOuterPadding(beginning: Int, end: Int, file: File) -> [Int] {
        let linesBeforeBeginning = beginning - 1..<beginning - 1 - configuration.outerPadding.beginning
        let linesAfterEnd = end + 1..<end + 1 + configuration.outerPadding.end
        return check(range: linesBeforeBeginning, file: file) + check(range: linesAfterEnd, file: file)
    }

    private func checkInnerPadding(beginning: Int, end: Int, file: File) -> [Int] {
        let linesAfterBeginning = beginning + 1..<beginning + 1 + configuration.innerPadding.beginning
        let linesBeforeEnd = end - 1..<end - 1 - configuration.innerPadding.end
        return check(range: linesAfterBeginning, file: file) + check(range: linesBeforeEnd, file: file)
    }

    private func check(range: CountableRange<Int>, file: File) -> [Int] {
        return range.filter {
            $0 < file.lines.count &&
            $0 >= 0 &&
            !isLineEmpty($0, file: file)
        }.map { $0 + 1 }
    }

    private func isLineEmpty(_ lineIndex: Int, file: File) -> Bool {
        let line = file.lines[lineIndex].content.trimmingCharacters(in: .whitespaces)
        guard line != "}" && line != "]" else { return false }
        let lineKinds: [SyntaxKind] = file.syntaxTokensByLines[lineIndex + 1].flatMap { SyntaxKind(rawValue: $0.type) }
        let commentKinds = SyntaxKind.commentKinds()
        return lineKinds.reduce(true) { $0.0 && commentKinds.contains($0.1) }
    }

    public func correct(file: File) -> [Correction] {
        let ranges: [StyleViolation] = validate(file: file)
        guard !ranges.isEmpty else { return [] }
        var lines: [String] = file.lines.map { $0.content }
        var corrections = [Correction]()

        ranges.sorted { $0.location > $1.location }.forEach {
            if let line = $0.location.line {
                let lineRange = file.lines[line].range
                if !file.ruleEnabled(violatingRanges: [lineRange], for: self).isEmpty {
                    lines.insert("", at: line)
                    corrections.append(Correction(ruleDescription: type(of: self).description, location: $0.location))
                }
            }
        }

        file.write(lines.joined(separator: "\n") + "\n")
        return corrections
    }
}
