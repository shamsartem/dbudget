import HyperList from 'hyperlist'
import { isLinkEl } from '../type-helpers'

const getListEl = (currentItem?: DisplayedTransaction): HTMLDivElement => {
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

export default (app: ElmApp): void => {
  const listClickListener = (e: Event): void => {
    e.preventDefault()

    const maybeLinkEl = e.target
    if (isLinkEl(maybeLinkEl)) {
      const linkEl = maybeLinkEl
      app.ports.clickedHyperListLink.send(linkEl.href)
    }
  }

  app.ports.onTransactionListInit.subscribe(list => {
    const el = document.getElementsByClassName('TransactionsList_list')[0]
    if (!el) return

    el.addEventListener('click', listClickListener)

    new HyperList(el, {
      itemHeight: 70,
      total: list.length,
      generate(index) {
        return getListEl(list[index])
      },
    })
  })
}
