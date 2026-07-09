const DEFAULT_DURATION = 300;

class AnimatedHeight extends HTMLElement {
    private observer: ResizeObserver;
    private lastHeight: number | null = null;
    private currentAnimation: Animation | null = null;
    private ignoreResizes = false;
    private duration = DEFAULT_DURATION;

    static get observedAttributes(): string[] {
        return ["duration"];
    }

    attributeChangedCallback(
        name: string,
        _oldValue: string | null,
        newValue: string | null,
    ): void {
        if (name === "duration" && newValue !== null) {
            const parsed = parseInt(newValue, 10);
            this.duration = Number.isFinite(parsed) && parsed >= 0
                ? parsed
                : DEFAULT_DURATION;
        }
    }

    constructor() {
        super();
        this.observer = new ResizeObserver((entries) => {
            if (this.ignoreResizes) return;
            const entry = entries[entries.length - 1];
            const newHeight =
                entry.borderBoxSize?.[0]?.blockSize ??
                entry.contentRect.height;
            this.onContentResize(newHeight);
        });
    }

    connectedCallback(): void {
        this.style.overflow = "hidden";
        this.style.display = "block";

        // Wait one frame for the initial children to be rendered before
        // recording the baseline height and starting observation. This avoids
        // an unwanted expand animation on first mount.
        requestAnimationFrame(() => {
            if (!this.isConnected) return;
            this.lastHeight = this.getBoundingClientRect().height;
            this.observer.observe(this);
        });
    }

    disconnectedCallback(): void {
        this.observer.disconnect();
        if (this.currentAnimation) {
            this.currentAnimation.cancel();
            this.currentAnimation = null;
        }
        this.ignoreResizes = false;
    }

    private onContentResize(newHeight: number): void {
        if (this.lastHeight === null) {
            this.lastHeight = newHeight;
            return;
        }

        const fromHeight = this.lastHeight;
        this.lastHeight = newHeight;

        if (Math.abs(newHeight - fromHeight) < 1) return;

        // If a previous animation is still running, start the new one from
        // the current visual position rather than jumping back to fromHeight.
        let startHeight = fromHeight;
        if (this.currentAnimation) {
            startHeight = this.getBoundingClientRect().height;
            this.currentAnimation.cancel();
            this.currentAnimation = null;
        }

        // Suppress ResizeObserver callbacks while WAAPI animates the height,
        // which would otherwise create a feedback loop.
        this.ignoreResizes = true;

        const animation = this.animate(
            [
                { height: `${startHeight}px` },
                { height: `${newHeight}px` },
            ],
            {
                duration: this.duration,
                easing: "ease-out",
                fill: "forwards",
            },
        );

        this.currentAnimation = animation;

        animation.finished
            .then(() => {
                // Cancel releases the fill, returning height to its natural
                // auto value (which equals newHeight at this point).
                animation.cancel();
                this.currentAnimation = null;
                this.ignoreResizes = false;
            })
            .catch(() => {
                // Animation was cancelled before finishing (e.g. a rapid
                // second transition or disconnection) — nothing to clean up.
                this.currentAnimation = null;
                this.ignoreResizes = false;
            });
    }
}

customElements.define("animated-height", AnimatedHeight);
