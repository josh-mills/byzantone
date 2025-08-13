class PitchTracker extends HTMLElement {
    constructor() {
        super();
        console.log("PitchTracker constructor called");

        this.innerHTML = "pitch tracker";

        // Add example click event listener
        this.addEventListener("click", this.handleClick);
    }

    // Handle click events on the component
    handleClick = () => {
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
