const clickOutside = (node: HTMLElement) => {
  let firstClick = true
  let clickedOnNode = false

  node.addEventListener('click', () => {
    clickedOnNode = true
  })

  const handleBodyClick = () => {
    if (firstClick || clickedOnNode) {
      firstClick = false
      clickedOnNode = false
      return
    }

    node.dispatchEvent(new CustomEvent('clickoutside'))
  }

  document.body.addEventListener('click', handleBodyClick)

  return {
    destroy() {
      document.body.removeEventListener('click', handleBodyClick)
    },
  }
}

export default clickOutside
