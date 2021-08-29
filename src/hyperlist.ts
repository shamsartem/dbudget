export type DisplayedTransaction = {
  name: string
  category: string
  date: string
  price: string
  id: string
  isIncome: boolean
}

const isLinkEl = (el: unknown): el is HTMLLinkElement =>
  Boolean(el && typeof el === 'object' && 'href' in el)

export const getListClickListener = (app: any) => (e: Event) => {
  e.preventDefault()
  const maybeLinkEl = e.target
  if (isLinkEl(maybeLinkEl)) {
    const linkEl = maybeLinkEl
    app.ports.clickedHyperlistLink.send(linkEl.href)
  }
}

export const getListEl = (currentItem?: DisplayedTransaction) => {
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

// const getSectionEl = (leftText: string, rightText: string) => {
//   const section = document.createElement('div')
//   section.classList.add('TransactionsList_itemSection')
//   const leftEl = document.createElement('div')
//   leftEl.innerHTML = leftText
//   const rightEl = document.createElement('div')
//   rightEl.innerHTML = rightText
//   section.appendChild(leftEl)
//   section.appendChild(rightEl)
//   section.setAttribute('aria-hidden', 'true')

//   return section
// }

// export const getListEl = (currentItem?: DisplayedTransaction) => {
//   if (!currentItem) {
//     const el = document.createElement('div')
//     el.innerHTML = 'Sync issue'
//     return el
//   }
//   const { category, date, isIncome, name, price, id } = currentItem
//   const item = document.createElement('div')
//   item.classList.add('TransactionsList_item')
//   if (isIncome) {
//     item.classList.add('isIncome')
//   }

//   const topSection = getSectionEl(date, category)
//   item.appendChild(topSection)

//   const bottomSection = getSectionEl(name, price)
//   item.appendChild(bottomSection)

//   const link = document.createElement('a')
//   link.href = `/transaction/${id}`
//   link.classList.add('TransactionsList_itemLink')

//   const linkText = document.createElement('div')
//   linkText.innerHTML = `${name}, ${price}, ${date}, ${category}`
//   linkText.classList.add('visuallyHidden')

//   link.appendChild(linkText)
//   item.appendChild(link)

//   return item
// }
