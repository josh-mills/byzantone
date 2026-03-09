const express = require("express");
const app = express();
const os = require("os");
const port = 3002;
// TODO: should get better environment configuration here
const LOCAL_DEV = false;

// Serve static files from the 'public' directory
app.use(express.static("public"));

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
