class PitchTracker extends HTMLElement {
    constructor() {
        super();
        console.log("PitchTracker constructor called");
    }

    connectedCallback() {
        console.log("PitchTracker connected");

        this.addEventListener("click", this.handleClick);
        this.innerHTML = "pitch tracker here!";
    }

    disconnectedCallback() {
        console.log("PitchTracker disconnected");

        this.removeEventListener("click", this.handleClick);
    }

    // Handle click events on the component. Just a template example.
    handleClick = () => {
        console.log("PitchTracker clicked");

        // Dispatch a custom event that Elm can listen to
        const event = new CustomEvent("pitchTrackerClicked", {
            bubbles: true,
            composed: true, // Allows the event to cross the shadow DOM boundary
            detail: { message: "Pitch tracker was clicked" },
        });

        this.dispatchEvent(event);
    };
}

window.customElements.define("pitch-tracker", PitchTracker);
