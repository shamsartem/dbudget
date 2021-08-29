const isLinkEl = (el: unknown): el is HTMLLinkElement =>
  Boolean(el && typeof el === 'object' && 'href' in el)

export const getListClickListener =
  (app: ElmApp) =>
  (e: Event): void => {
    e.preventDefault()

    const maybeLinkEl = e.target
    if (isLinkEl(maybeLinkEl)) {
      const linkEl = maybeLinkEl
      app.ports.clickedHyperListLink.send(linkEl.href)
    }
  }

export const getListEl = (
  currentItem?: DisplayedTransaction,
): HTMLDivElement => {
  if (!currentItem) {
    const el = document.createElement('div')
    el.innerHTML = 'Sync issue'
    return el
  }

  const { category, date, isIncome, name, price, id } = currentItem

  const item = document.createElement('div')
  item.classList.add('TransactionsList_item')

  if (isIncome) {
    item.classList.add('isIncome')
  }

  item.innerHTML = `
    <div class="TransactionsList_itemSection" aria-hidden="true">
      <div>${date}</div>
      <div>${category}</div>
    </div>
    <div class="TransactionsList_itemSection" aria-hidden="true">
      <div>${name}</div>
      <div>${price}</div>
    </div>
    <a href="/transaction/${id}" class="TransactionsList_itemLink" id="${id}">
      <div class="visuallyHidden">
        ${name}, ${price}, ${date}, ${category}
      </div>
    </a>
  `.trim()

  return item
}
