const portal = (node: HTMLElement) => {
  document.body.appendChild(node)
  return {
    destroy() {
      node.remove()
    },
  }
}

export default portal
