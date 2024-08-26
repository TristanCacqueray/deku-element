export const defineComponent =
  (componentName) => (attrs) => (setupWires) => (setupElement) => (attributeChangedHandler) => () => {
    class CustomComponent extends HTMLElement {
      static observedAttributes = attrs
      constructor() {
        super();
        this.attachShadow({ mode: "open" });
        this.wires = setupWires();
        setupElement(this.wires)(this.shadowRoot)()
      }
      attributeChangedCallback(name, oldValue, newValue) {
        attributeChangedHandler(this.wires)(name)(newValue)()
      }
    }
    window.customElements.define(componentName, CustomComponent);
  };
