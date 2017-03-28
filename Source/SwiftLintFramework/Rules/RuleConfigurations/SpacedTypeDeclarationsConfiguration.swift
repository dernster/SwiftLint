//
//  SpacedTypeDeclarationConfiguration.swift
//  SwiftLint
//
//  Created by Diego Ernst on 3/22/17.
//  Copyright © 2017 Realm. All rights reserved.
//

import Foundation

struct Padding: Equatable {

    var beginning: Int
    var end: Int

    static var `default`: Padding {
        return Padding(beginning: 1, end: 1)
    }

    var description: String {
        return "(beginning: \(beginning), end: \(end))"
    }

    static func create(from configuration: Any) -> Padding? {
        if let symmetricPadding = configuration as? Int {
            return Padding(beginning: symmetricPadding, end: symmetricPadding)
        } else if
            let dictionary = configuration as? [String: Int],
            let beginning = dictionary["beginning"],
            let end = dictionary["end"] {

            return Padding(beginning: beginning, end: end)
        }
        return nil
    }

    public static func == (lhs: Padding, rhs: Padding) -> Bool {
        return lhs.beginning == rhs.beginning && lhs.end == rhs.end
    }

}

public struct SpacedTypeDelcarationsConfiguration: RuleConfiguration, Equatable {
    var severityConfiguration = SeverityConfiguration(.warning)
    var outerPadding: Padding = .default
    var innerPadding: Padding = .default

    public var consoleDescription: String {
        return severityConfiguration.consoleDescription +
            ", outer_padding: \(outerPadding.description)" +
            ", inner_padding: \(innerPadding.description)"
    }

    public mutating func apply(configuration: Any) throws {
        guard
            let configuration = configuration as? [String: Any],
            let outerPadding = Padding.create(from: configuration["outer_padding"] ?? [:]),
            let innerPadding = Padding.create(from: configuration["inner_padding"] ?? [:])
        else {
            throw ConfigurationError.unknownConfiguration
        }

        self.outerPadding = outerPadding
        self.innerPadding = innerPadding

        if let severityString = configuration["severity"] as? String {
            try severityConfiguration.apply(configuration: severityString)
        }
    }

    public static func == (lhs: SpacedTypeDelcarationsConfiguration,
                           rhs: SpacedTypeDelcarationsConfiguration) -> Bool {
        return lhs.severityConfiguration == rhs.severityConfiguration &&
            lhs.outerPadding == rhs.outerPadding &&
            rhs.innerPadding == rhs.innerPadding
    }
}
