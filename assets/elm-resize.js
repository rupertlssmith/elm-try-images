class Resizeable extends HTMLElement {
    constructor() {
        super();

        this.resizeCallback = this.resizeCallback.bind(this);
        this._observer = new ResizeObserver(this.resizeCallback);
    }

    connectedCallback() {
        this._observer.observe(this);
    }

    disconnectedCallback() {
        this._observer.disconnect();
    }

    resizeCallback(e) {
        for (let entry of e) {
            //console.log(entry);

            if (entry.borderBoxSize) {
                // Firefox implements as a single content rect, rather than an array
                const borderBoxSize = Array.isArray(entry.borderBoxSize) ? entry.borderBoxSize[0] : entry.borderBoxSize;

                // console.log("contentBoxSize.inlineSize");
                // console.log(borderBoxSize.inlineSize);

                // console.log("contentBoxSize.blockSize");
                // console.log(borderBoxSize.blockSize);

                let event = new CustomEvent("resize", {
                    detail: {
                        w: borderBoxSize.inlineSize,
                        h: borderBoxSize.blockSize
                    }
                });

                this.dispatchEvent(event);
            }
        }
    };
}

customElements.define('elm-resize', Resizeable);
