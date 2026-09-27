const DEFAULT_DURATION = 300;

class AnimatedHeight extends HTMLElement {
    private observer: ResizeObserver;
    private lastHeight: number | null = null;
    private currentAnimation: Animation | null = null;
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
            const entry = entries[entries.length - 1];
            this.onContentResize(entry.contentRect.height);
        });
    }

    connectedCallback(): void {
        // Observe the first child rather than this element itself. The child's
        // height is driven purely by its content and is unaffected by WAAPI
        // animating this element's height, so no feedback loop is possible.
        // Waiting one frame lets Elm render the initial child before we begin.
        requestAnimationFrame(() => {
            if (!this.isConnected) return;
            const child = this.firstElementChild;
            if (!child) return;
            this.lastHeight = child.getBoundingClientRect().height;
            this.observer.observe(child);
        });
    }

    disconnectedCallback(): void {
        this.observer.disconnect();
        if (this.currentAnimation) {
            this.currentAnimation.cancel();
            this.currentAnimation = null;
        }
    }

    private onContentResize(newHeight: number): void {
        if (this.lastHeight === null) {
            this.lastHeight = newHeight;
            return;
        }

        const fromHeight = this.lastHeight;
        this.lastHeight = newHeight;

        if (Math.abs(newHeight - fromHeight) < 1) return;

        if (window.matchMedia("(prefers-reduced-motion: reduce)").matches) return;

        // If a previous animation is still running, read the current visual
        // position before cancelling so the new animation starts from there.
        let startHeight = fromHeight;
        if (this.currentAnimation) {
            startHeight = this.getBoundingClientRect().height;
            this.currentAnimation.cancel();
            this.currentAnimation = null;
        }

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
                if (this.currentAnimation !== animation) return;
                // Cancel releases the fill, returning height to its natural
                // auto value (which equals newHeight at this point).
                animation.cancel();
                this.currentAnimation = null;
            })
            .catch(() => {
                if (this.currentAnimation !== animation) return;
                this.currentAnimation = null;
            });
    }
}

customElements.define("animated-height", AnimatedHeight);
