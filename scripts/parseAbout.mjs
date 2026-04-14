import fs from "fs";
import path from "path";

/**
 * Parses about.md and generates a structured JSON representation
 */
export function parseAbout(aboutPath, outputPath) {
    const content = fs.readFileSync(aboutPath, "utf-8").trim();

    const about = {
        content,
        lastUpdated: new Date().toISOString(),
    };

    fs.writeFileSync(outputPath, JSON.stringify(about, null, 2));
    console.log(`About content parsed and saved to ${outputPath}`);

    return about;
}

// If run directly
if (import.meta.url === `file://${process.argv[1]}`) {
    const aboutPath = "static-content/about.md";
    const outputPath = "public/dist/about.json";

    // Ensure output directory exists
    fs.mkdirSync(path.dirname(outputPath), { recursive: true });

    parseAbout(aboutPath, outputPath);
}
