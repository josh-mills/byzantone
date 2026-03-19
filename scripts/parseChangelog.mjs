import { marked } from "marked";
import fs from "fs";
import path from "path";

/**
 * Parses CHANGELOG.md and generates a structured JSON representation
 */
export function parseChangelog(changelogPath, outputPath) {
    const changelogContent = fs.readFileSync(changelogPath, "utf-8");
    const tokens = marked.lexer(changelogContent);

    const changelog = {
        entries: [],
        lastUpdated: new Date().toISOString(),
    };

    let currentEntry = null;
    let currentSection = null;

    for (const token of tokens) {
        // Stop parsing at horizontal rule (---)
        if (token.type === "hr") {
            break;
        }

        if (token.type === "heading") {
            if (token.depth === 2) {
                // New version entry
                if (currentEntry) {
                    changelog.entries.push(currentEntry);
                }

                const headerText = token.text;

                // Skip ellipsis entries
                if (headerText.trim() === "...") {
                    currentEntry = null;
                    currentSection = null;
                    continue;
                }

                const versionMatch = headerText.match(
                    /v?(\d+\.\d+\.\d+|\?\.\?\.\d+|\?\.\?\.0)/i,
                );
                const dateMatch = headerText.match(/(\d{4}-\d{2}-\d{2})/);

                // Require date for all entries
                if (!dateMatch) {
                    console.error(
                        `Error: No date found for version "${headerText}". All releases must have dates in YYYY-MM-DD format.`,
                    );
                    process.exit(1);
                }

                currentEntry = {
                    version: versionMatch ? versionMatch[1] : headerText,
                    date: dateMatch[1],
                    changes: {
                        added: [],
                        changed: [],
                        deprecated: [],
                        removed: [],
                        fixed: [],
                        security: [],
                    },
                };
                currentSection = null;
            } else if (token.depth === 3 && currentEntry) {
                // Change category section
                currentSection = token.text.toLowerCase();
            }
        } else if (token.type === "list" && currentEntry && currentSection) {
            // Extract list items for current section
            const items = token.items.map((item) => item.text.trim());
            if (currentEntry.changes[currentSection]) {
                currentEntry.changes[currentSection].push(...items);
            }
        }
    }

    // Don't forget the last entry
    if (currentEntry) {
        changelog.entries.push(currentEntry);
    }

    // Write to output file
    fs.writeFileSync(outputPath, JSON.stringify(changelog, null, 2));
    console.log(`Changelog parsed and saved to ${outputPath}`);

    return changelog;
}

// If run directly
if (import.meta.url === `file://${process.argv[1]}`) {
    const changelogPath = "CHANGELOG.md";
    const outputPath = "public/dist/changelog.json";

    // Ensure output directory exists
    fs.mkdirSync(path.dirname(outputPath), { recursive: true });

    parseChangelog(changelogPath, outputPath);
}
