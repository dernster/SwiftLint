//
//  LineAfterDeclarationRule.swift
//  SwiftLint
//
//  Created by Diego Ernst on 1/31/17.
//  Copyright © 2017 Realm. All rights reserved.
//

import Foundation
import SourceKittenFramework

struct LineError: Equatable {
    let line: Int
    let message: String

    public static func == (lhs: LineError, rhs: LineError) -> Bool {
        return lhs.line == rhs.line && lhs.message == rhs.message
    }
}

public struct SpacedTypeDeclarationsRule: ASTRule, OptInRule, ConfigurationProviderRule, CorrectableRule {
    public var configuration = SpacedTypeDelcarationsConfiguration()
    private var declarationTypes: [SwiftDeclarationKind] = [.class, .enum, .struct, .protocol, .extension]

    static let identifier = "spaced_type_declarations"
    static let name = "Spaced Type Declarations"

    public static let description = RuleDescription(
        identifier: identifier,
        name: name,
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

    static let lineShouldBeEmpty = "Insert a new line at the end of this line!"
    static let lineShouldNotBeEmpty = "Line should not be empty!"

    public init() {}

    public func validate(file: File) -> [StyleViolation] {
        let violations = validate(file: file, dictionary: file.structure.dictionary)
        return violations.unique
    }

    public func validate(file: File, kind: SwiftDeclarationKind,
                         dictionary: [String: SourceKitRepresentable]) -> [StyleViolation] {
        guard
            declarationTypes.contains(kind),
            let bodyOffset = dictionary.bodyOffset,
            let bodyLength = dictionary.bodyLength,
            let (startLine, _) = file.contents.bridge().lineAndCharacter(forByteOffset: bodyOffset),
            let (endLine, _) = file.contents.bridge().lineAndCharacter(forByteOffset: bodyOffset + bodyLength)
        else {
            return []
        }
        return findErrors(startLine: startLine, endLine: endLine, file: file)
    }

    private func findErrors(startLine: Int, endLine: Int, file: File) -> [StyleViolation] {
        var lineErrors = [LineError]()
        lineErrors.append(contentsOf: checkOuterPadding(beginning: startLine - 1, end: endLine - 1, file: file))
        lineErrors.append(contentsOf: checkInnerPadding(beginning: startLine - 1, end: endLine - 1, file: file))
        return lineErrors.unique.map { StyleViolation(
                ruleDescription: RuleDescription(
                    identifier: SpacedTypeDeclarationsRule.identifier,
                    name: SpacedTypeDeclarationsRule.name,
                    description: $0.message
                ),
                severity: self.configuration.severityConfiguration.severity,
                location: Location(file: file.path, line: $0.line, character: 1)
            )
        }
    }

    private func checkOuterPadding(beginning: Int, end: Int, file: File) -> [LineError] {
        let linesBeforeBeginning = beginning - configuration.outerPadding.beginning..<beginning
        let linesAfterEnd = end + 1..<end + 1 + configuration.outerPadding.end
        return check(range: linesBeforeBeginning, file: file) + check(range: linesAfterEnd, file: file)
    }

    private func checkInnerPadding(beginning: Int, end: Int, file: File) -> [LineError] {
        guard end > beginning + 1 else { return [] }
        let linesAfterBeginning = beginning + 1..<beginning + 1 + configuration.innerPadding.beginning
        let linesBeforeEnd = end - configuration.innerPadding.end..<end
        return check(range: linesAfterBeginning, file: file) + check(range: linesBeforeEnd, file: file)
    }

    private func check(range: CountableRange<Int>, file: File) -> [LineError] {
        var linesWithErrors: [LineError] = range.filter {
            $0 < file.lines.count &&
            $0 >= 0 &&
            !isLineEmpty($0, file: file)
        }.map { LineError(line: $0 + 1, message: SpacedTypeDeclarationsRule.lineShouldBeEmpty) }

        if range.lowerBound > 0 && isLineEmpty(range.lowerBound - 1, file: file) {
            linesWithErrors.append(LineError(line: range.lowerBound, message: SpacedTypeDeclarationsRule.lineShouldNotBeEmpty))
        }

        if range.upperBound < file.lines.count && isLineEmpty(range.upperBound, file: file) {
            linesWithErrors.append(LineError(line: range.upperBound + 1, message: SpacedTypeDeclarationsRule.lineShouldNotBeEmpty))
        }
        return linesWithErrors
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
