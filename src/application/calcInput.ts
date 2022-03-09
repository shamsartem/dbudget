const calcOnly = (string: string) =>
  string.replace(',', '.').replace(/[^\d+\-.]/g, '')

const inputEvent = new Event('input', {
  bubbles: true,
  cancelable: true,
})

window.addEventListener(
  'input',
  (event) => {
    const target = event.target as HTMLInputElement

    if (!target?.dataset || !('calcInput' in target.dataset)) {
      return
    }

    const pos = target.selectionStart

    if (!pos) {
      return
    }

    const before = calcOnly(target.value.slice(0, pos))
    const after = calcOnly(target.value.slice(pos))
    target.value = before + after
    target.selectionStart = before.length
    target.selectionEnd = before.length
  },
  true,
)

window.addEventListener(
  'blur',
  (event) => {
    const target = event.target as HTMLInputElement

    if (!target?.dataset || !('calcInput' in target.dataset)) {
      return
    }

    const pos = target.selectionStart

    if (!pos) {
      return
    }

    target.dataset.pos = `${pos}`
  },
  { capture: true },
)

const insertSymbol = (inputEl: HTMLInputElement, symbol: string) => {
  const pos = +(inputEl.dataset.pos || inputEl.value.length)
  const before = inputEl.value.slice(0, pos) + symbol
  const after = calcOnly(inputEl.value.slice(pos))
  inputEl.value = before + after
  inputEl.dispatchEvent(inputEvent)
  inputEl.focus()
  inputEl.selectionStart = before.length
  inputEl.selectionEnd = before.length
}

window.addEventListener(
  'click',
  (event) => {
    const target = event.target as HTMLButtonElement
    const inputId = target?.dataset?.forCalcInput

    if (!inputId) {
      return
    }

    const inputEl = document.getElementById(inputId) as HTMLInputElement | null

    if (!inputEl) {
      return
    }

    if ('plus' in target.dataset) {
      insertSymbol(inputEl, '+')
      return
    }

    if ('minus' in target.dataset) {
      insertSymbol(inputEl, '-')
      return
    }
  },
  true,
)
