import { hasKey, hasKeys } from './typeHelpers'

const calcOnly = (string: string): string =>
  string.replace(',', '.').replace(/[^\d+\-.]/g, '')

const inputEvent = new Event('input', {
  bubbles: true,
  cancelable: true,
})

window.addEventListener(
  'input',
  ({ target }): void => {
    if (
      !(target instanceof HTMLInputElement) ||
      !hasKey(target.dataset, 'calcInput') ||
      typeof target.value !== 'string' ||
      typeof target.selectionStart !== 'number'
    ) {
      return
    }

    const before = calcOnly(target.value.slice(0, target.selectionStart))
    const after = calcOnly(target.value.slice(target.selectionStart))
    target.value = before + after
    target.selectionStart = before.length
    target.selectionEnd = before.length
  },
  true,
)

window.addEventListener(
  'blur',
  ({ target }): void => {
    if (
      !(target instanceof HTMLInputElement) ||
      !hasKeys(target.dataset, 'calcInput', 'pos') ||
      typeof target.selectionStart !== 'number'
    ) {
      return
    }

    target.dataset.pos = `${target.selectionStart}`
  },
  { capture: true },
)

const insertSymbol = (
  inputEl: HTMLInputElement & { dataset: { pos?: string } },
  symbol: string,
): void => {
  const pos = +(inputEl?.dataset.pos ?? inputEl.value.length)
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
  ({ target }): void => {
    if (
      !hasKeys(target, 'dataset') ||
      !hasKeys(target.dataset, 'forCalcInput') ||
      typeof target.dataset.forCalcInput !== 'string'
    ) {
      return
    }

    const inputEl = document.getElementById(target.dataset.forCalcInput)

    if (!(inputEl instanceof HTMLInputElement)) {
      return
    }

    if (hasKey(target.dataset, 'plus')) {
      insertSymbol(inputEl, '+')
      return
    }

    if (hasKey(target.dataset, 'minus')) {
      insertSymbol(inputEl, '-')
      return
    }
  },
  true,
)
