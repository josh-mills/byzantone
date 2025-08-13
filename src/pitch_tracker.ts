class PitchTracker extends HTMLElement {
    constructor() {
        super();
        console.log("PitchTracker constructor called");

        this.innerHTML = "pitch tracker!";
    }
}

window.customElements.define("pitch-tracker", PitchTracker);
