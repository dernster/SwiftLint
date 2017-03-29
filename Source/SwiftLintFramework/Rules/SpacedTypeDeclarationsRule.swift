//
//  LineAfterDeclarationRule.swift
//  SwiftLint
//
//  Created by Diego Ernst on 1/31/17.
//  Copyright © 2017 Realm. All rights reserved.
//

import Foundation
import SourceKittenFramework

fileprivate class LineCorrections {

    enum Correction: String {
        case shouldBeEmpty = "Line should be empty"
        case shouldNotBeEmpty = "Line should not be empty"

        func verify(file: File, lineIndex: Int) -> Bool {
            switch self {
            case .shouldBeEmpty:
                return LineCorrections.isLineEmpty(lineIndex, file: file)
            case .shouldNotBeEmpty:
                return !LineCorrections.isLineEmpty(lineIndex, file: file)
            }
        }
    }

    private var correction = [Int: Correction]()
    var priority: Priority = .default

    subscript(lineIndex: Int, file: File) -> Correction? {
        get {
            return correction[lineIndex]
        }
        set {
            guard let newValue = newValue else { return }
            guard let currentCorrection = correction[lineIndex] else {
                correction[lineIndex] = newValue
                return
            }
            // hay que hacer override solo si ambas no se cumplen.
//            let currentCorrectionVerifies = currentCorrection.verify(file: file, lineIndex: lineIndex)
//            let newCorrectionVerifies = newValue.verify(file: file, lineIndex: lineIndex)
//            if !currentCorrectionVerifies && !newCorrectionVerifies {
                let highPriorityCorrection: Correction = priority == .emptyLines ? .shouldBeEmpty : .shouldNotBeEmpty
                correction[lineIndex] = currentCorrection == highPriorityCorrection ? highPriorityCorrection : newValue
//            } else if currentCorrectionVerifies {
//                correction[lineIndex] = newValue
//            }
        }
    }

    func violations(file: File, severity: ViolationSeverity) -> [StyleViolation] {
        var filteredCorrections = [Int: Correction]()
        correction.forEach { key, value in
            guard value == .shouldBeEmpty && correction[key - 1] == .shouldBeEmpty else {
                filteredCorrections[key] = value
                return
            }
        }
        return filteredCorrections.flatMap { lineIndex, correction in
            guard !correction.verify(file: file, lineIndex: lineIndex) else { return nil }
            return StyleViolation(
                    ruleDescription: RuleDescription(
                        identifier: SpacedTypeDeclarationsRule.identifier,
                        name: SpacedTypeDeclarationsRule.name,
                        description: correction.rawValue
                    ),
                    severity: severity,
                    location: Location(file: file.path, line: lineIndex + 1, character: 1)
                )
            }
    }

    static func isLineEmpty(_ lineIndex: Int, file: File) -> Bool {
        let line = file.lines[lineIndex].content.trimmingCharacters(in: .whitespaces)
        guard line != "}" && line != "]" else { return false }
        let lineKinds: [SyntaxKind] = file.syntaxTokensByLines[lineIndex + 1].flatMap { SyntaxKind(rawValue: $0.type) }
        let commentKinds = SyntaxKind.commentKinds()
        return lineKinds.reduce(true) { $0.0 && commentKinds.contains($0.1) }
    }

}

public struct SpacedTypeDeclarationsRule: Rule, OptInRule, ConfigurationProviderRule, CorrectableRule {
    public var configuration = SpacedTypeDelcarationsConfiguration() {
        didSet {
            lineCorrections.priority = configuration.priority
        }
    }

    private var lineCorrections = LineCorrections()

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

    public init() {}

    public func validate(file: File) -> [StyleViolation] {
        validate(file: file, dictionary: file.structure.dictionary)
        return lineCorrections.violations(file: file, severity: self.configuration.severityConfiguration.severity)
    }

    public func validate(file: File, dictionary: [String: SourceKitRepresentable]) {
        dictionary.substructure.forEach { subDict in
            validate(file: file, dictionary: subDict)
            if let kindString = subDict.kind, let kind = SwiftDeclarationKind(rawValue: kindString) {
                validate(file: file, kind: kind, dictionary: subDict)
            }
        }
    }

    public func validate(file: File, kind: SwiftDeclarationKind, dictionary: [String: SourceKitRepresentable]) {
        guard
            declarationTypes.contains(kind),
            let bodyOffset = dictionary.bodyOffset,
            let bodyLength = dictionary.bodyLength,
            let (startLine, _) = file.contents.bridge().lineAndCharacter(forByteOffset: bodyOffset),
            let (endLine, _) = file.contents.bridge().lineAndCharacter(forByteOffset: bodyOffset + bodyLength)
        else {
            return
        }
        findErrors(startLine: startLine, endLine: endLine, file: file)
    }

    private func findErrors(startLine: Int, endLine: Int, file: File) {
        checkOuterPadding(beginning: startLine - 1, end: endLine - 1, file: file)
        checkInnerPadding(beginning: startLine - 1, end: endLine - 1, file: file)
    }

    private func checkOuterPadding(beginning: Int, end: Int, file: File) {
        let linesBeforeBeginning = beginning - configuration.outerPadding.beginning..<beginning
        let linesAfterEnd = end + 1..<end + 1 + configuration.outerPadding.end
        check(range: linesBeforeBeginning, file: file, isOuter: true)
        check(range: linesAfterEnd, file: file, isOuter: true)
    }

    private func checkInnerPadding(beginning: Int, end: Int, file: File) {
        guard end > beginning + 1 else { return }
        let linesAfterBeginning = beginning + 1..<beginning + 1 + configuration.innerPadding.beginning
        let linesBeforeEnd = end - configuration.innerPadding.end..<end
        check(range: linesAfterBeginning, file: file, isOuter: false)
        check(range: linesBeforeEnd, file: file, isOuter: false)
    }

    private func check(range: CountableRange<Int>, file: File, isOuter: Bool) {
        range.filter { $0 < file.lines.count && $0 >= 0 }.forEach { lineCorrections[$0, file] = .shouldBeEmpty }
        guard isOuter else { return }
        if range.lowerBound - 1 >= 0 && range.lowerBound - 1 < file.lines.count {
            lineCorrections[range.lowerBound - 1, file] = .shouldNotBeEmpty
        }
        if range.upperBound >= 0 && range.upperBound < file.lines.count {
            lineCorrections[range.upperBound, file] = .shouldNotBeEmpty
        }
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
                    if $0.reason == LineCorrections.Correction.shouldBeEmpty.rawValue {
                        lines.insert("", at: line)
                    } else {
                        lines.remove(at: line - 1)
                    }
                    corrections.append(Correction(ruleDescription: type(of: self).description, location: $0.location))
                }
            }
        }

        file.write(lines.joined(separator: "\n") + "\n")
        return corrections
    }
}
