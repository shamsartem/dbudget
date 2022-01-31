export const isLinkEl = (el: unknown): el is HTMLLinkElement =>
  Boolean(el && typeof el === 'object' && 'href' in el)
