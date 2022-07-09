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
    if (!(target instanceof HTMLInputElement)) {
      return
    }

    if (!hasKey(target.dataset, 'calcInput')) {
      return
    }

    const { value } = target

    if (typeof value !== 'string') {
      return
    }

    const { selectionStart } = target

    if (typeof selectionStart !== 'number') {
      return
    }

    const before = calcOnly(value.slice(0, selectionStart))
    const after = calcOnly(value.slice(selectionStart))
    target.value = before + after
    target.selectionStart = before.length
    target.selectionEnd = before.length
  },
  true,
)

window.addEventListener(
  'blur',
  ({ target }): void => {
    if (!(target instanceof HTMLInputElement)) {
      return
    }

    if (!hasKeys(target.dataset, 'calcInput', 'pos')) {
      return
    }

    const { selectionStart } = target

    if (typeof selectionStart !== 'number') {
      return
    }

    target.dataset.pos = `${selectionStart}`
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
    if (!hasKeys(target, 'dataset')) {
      return
    }

    if (!hasKeys(target.dataset, 'forCalcInput')) {
      return
    }

    if (typeof target.dataset.forCalcInput !== 'string') {
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
