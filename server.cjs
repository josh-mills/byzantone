const express = require("express");
const app = express();
const os = require("os");
const fs = require("fs");
const path = require("path");
const port = 3002;
// TODO: should get proper environment configuration here
const LOCAL_DEV = false;

// Import the changelog parser (using dynamic import for ES modules)
let parseChangelog;
import("./scripts/parseChangelog.mjs").then((module) => {
    parseChangelog = module.parseChangelog;
    // Generate initial changelog
    generateChangelog();
});

function generateChangelog() {
    if (parseChangelog) {
        try {
            const changelogPath = "CHANGELOG.md";
            const outputPath = "public/dist/changelog.json";

            // Ensure directory exists
            fs.mkdirSync(path.dirname(outputPath), { recursive: true });

            parseChangelog(changelogPath, outputPath);
        } catch (error) {
            console.error("Error generating changelog:", error);
        }
    }
}

// Watch for changes to CHANGELOG.md in development
if (fs.existsSync("CHANGELOG.md")) {
    fs.watchFile("CHANGELOG.md", (curr, prev) => {
        console.log("CHANGELOG.md changed, regenerating...");
        generateChangelog();
    });
}

// Serve static files from the 'public' directory
app.use(express.static("public"));

// API endpoint for changelog (fallback if file doesn't exist)
app.get("/dist/changelog.json", (req, res) => {
    const changelogPath = "public/dist/changelog.json";

    if (fs.existsSync(changelogPath)) {
        res.sendFile(path.resolve(changelogPath));
    } else {
        // Generate on-demand if file doesn't exist
        generateChangelog();
        if (fs.existsSync(changelogPath)) {
            res.sendFile(path.resolve(changelogPath));
        } else {
            res.status(500).json({ error: "Could not generate changelog" });
        }
    }
});

function getLocalIP() {
    const interfaces = os.networkInterfaces();
    for (const name of Object.keys(interfaces)) {
        for (const iface of interfaces[name]) {
            // Skip internal and non-IPv4 addresses
            if (iface.family === "IPv4" && !iface.internal) {
                return iface.address;
            }
        }
    }
}

if (LOCAL_DEV) {
    // Start the server and accept connections from all network interfaces
    app.listen(port, "0.0.0.0", () => {
        console.log(`Server running on http://localhost:${port}`);
        console.log(
            `Also accessible from network at http://${getLocalIP()}:${port}`,
        );
    });
} else {
    app.listen(port, () => {
        console.log(`Server running on port ${port}`);
    });
}
