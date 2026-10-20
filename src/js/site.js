//////////////////////////////////////////////////////////////////////////////
///   DARK MODE   ////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

const HSThemeAppearance = {
    currentTheme: 'default',
    storageWriteFailed: false,
    init() {
        this.setAppearance(this.getOriginalAppearance(), false, false)
    },
    _resetStylesOnLoad() {
        const resetStyles = document.createElement('style')
        resetStyles.textContent = '*{transition: unset !important;}'
        resetStyles.setAttribute('data-hs-appearance-onload-styles', '')
        document.head.appendChild(resetStyles)
        return resetStyles
    },
    setAppearance(theme, saveInStore = true, dispatchEvent = true) {
        const requestedTheme = ['dark', 'default', 'auto'].includes(theme) ? theme : 'default'
        this.currentTheme = requestedTheme
        if (saveInStore) {
            try {
                localStorage.setItem('hs_theme', requestedTheme)
                this.storageWriteFailed = false
            } catch (_error) {
                this.storageWriteFailed = true
                // A denied or unavailable storage API must not disable controls.
            }
        }

        const appearance = requestedTheme === 'auto'
            ? (window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'default')
            : requestedTheme
        const root = document.documentElement
        root.classList.toggle('dark', appearance === 'dark')
        root.classList.toggle('default', appearance === 'default')
        root.classList.toggle('auto', requestedTheme === 'auto')

        const themeToggle = document.querySelector('#theme-toggle')
        if (themeToggle) themeToggle.setAttribute('aria-pressed', String(appearance === 'dark'))

        const resetStyles = this._resetStylesOnLoad()
        window.setTimeout(() => resetStyles.remove())

        if (dispatchEvent) {
            window.dispatchEvent(new CustomEvent('on-hs-appearance-change', { detail: appearance }))
        }
        return appearance
    },
    getAppearance() {
        const theme = this.currentTheme
        if (theme === 'auto') {
            return window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'default'
        }
        return theme
    },
    getOriginalAppearance() {
        if (this.storageWriteFailed) return this.currentTheme
        try {
            const theme = localStorage.getItem('hs_theme')
            return ['dark', 'default', 'auto'].includes(theme) ? theme : 'default'
        } catch (_error) {
            return this.currentTheme
        }
    }
}

HSThemeAppearance.init()

const colorScheme = window.matchMedia('(prefers-color-scheme: dark)')
colorScheme.addEventListener?.('change', () => {
    if (HSThemeAppearance.getOriginalAppearance() === 'auto') {
        HSThemeAppearance.setAppearance('auto', false)
    }
})

document.querySelector('#theme-toggle')?.addEventListener('click', () => {
    const nextTheme = HSThemeAppearance.getAppearance() === 'dark' ? 'default' : 'dark'
    HSThemeAppearance.setAppearance(nextTheme)
})

//////////////////////////////////////////////////////////////////////////////
///   NATIVE NAVIGATION AND DISCLOSURE   /////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

const navMedia = window.matchMedia('(min-width: 40rem)')
const mobileNav = document.querySelector('#navbar-collapse-with-animation')
const mobileNavToggle = document.querySelector('#mobile-navigation-toggle')
const mobileMenuIcon = document.querySelector('.mobile-menu-icon')
const mobileCloseIcon = document.querySelector('.mobile-close-icon')
const downloadToggle = document.querySelector('#hs-dropdown-download')
const downloadMenu = document.querySelector('#download-menu')
const downloadDisclosure = downloadToggle?.closest('.custom-dropdown')
let lastNavigationFocus = null

function setMobileNav(open, returnFocus = false) {
    if (!mobileNav || !mobileNavToggle) return
    const expanded = !navMedia.matches && open
    mobileNav.classList.toggle('hidden', !expanded)
    mobileNavToggle.setAttribute('aria-expanded', String(expanded))
    mobileMenuIcon?.classList.toggle('hidden', expanded)
    mobileCloseIcon?.classList.toggle('hidden', !expanded)
    if (returnFocus) mobileNavToggle.focus()
}

function setDownloadOpen(open, returnFocus = false) {
    if (!downloadToggle || !downloadMenu) return
    downloadMenu.classList.toggle('hidden', !open)
    downloadToggle.setAttribute('aria-expanded', String(open))
    if (returnFocus) downloadToggle.focus()
}

mobileNavToggle?.addEventListener('click', () => {
    setMobileNav(mobileNavToggle.getAttribute('aria-expanded') !== 'true')
})

navMedia.addEventListener('change', (event) => {
    const activeElement = document.activeElement
    const activeIsDocument = activeElement === document.body || activeElement === document.documentElement
    const recentlyHiddenFocus = activeIsDocument
        && lastNavigationFocus?.getClientRects().length === 0
        ? lastNavigationFocus
        : null
    const focusedElement = activeIsDocument ? recentlyHiddenFocus : activeElement
    const focusedWithinMobileNav = mobileNav?.contains(focusedElement)
    const focusedMobileToggle = focusedElement === mobileNavToggle
    const focusedDownloadLink = downloadMenu?.contains(focusedElement)

    setMobileNav(false)
    setDownloadOpen(false)

    if (!event.matches && focusedWithinMobileNav) {
        mobileNavToggle?.focus()
    } else if (event.matches && focusedMobileToggle) {
        const firstNavigationLink = document.querySelector('#lfe-main-nav > a[href]')
            || document.querySelector('nav.top-nav a.brand-plate')
        firstNavigationLink?.focus()
    } else if (focusedDownloadLink) {
        downloadToggle?.focus()
    }
})

downloadToggle?.addEventListener('click', () => {
    setDownloadOpen(downloadToggle.getAttribute('aria-expanded') !== 'true')
})

document.addEventListener('pointerdown', (event) => {
    if (downloadDisclosure && !downloadDisclosure.contains(event.target)) {
        setDownloadOpen(false)
    }
    if (!mobileNav?.contains(event.target) && event.target !== mobileNavToggle) {
        lastNavigationFocus = null
    }
})

document.addEventListener('focusin', (event) => {
    if (mobileNav?.contains(event.target) || event.target === mobileNavToggle) {
        lastNavigationFocus = event.target
    } else if (event.target !== document.body && event.target !== document.documentElement) {
        lastNavigationFocus = null
    }
    if (downloadDisclosure && !downloadDisclosure.contains(event.target)) {
        setDownloadOpen(false)
    }
})

document.addEventListener('keydown', (event) => {
    if (event.key !== 'Escape') return

    const activeElement = document.activeElement
    const focusWithinDownload = downloadDisclosure?.contains(event.target)
        || downloadDisclosure?.contains(activeElement)
    if (downloadToggle?.getAttribute('aria-expanded') === 'true' && focusWithinDownload) {
        event.preventDefault()
        setDownloadOpen(false, true)
        return
    }

    const focusWithinMobileNav = mobileNav?.contains(event.target)
        || mobileNav?.contains(activeElement)
    if (!navMedia.matches && mobileNavToggle?.getAttribute('aria-expanded') === 'true' && focusWithinMobileNav) {
        event.preventDefault()
        setMobileNav(false, true)
    }
})

//////////////////////////////////////////////////////////////////////////////
///   NATIVE HOMEPAGE TABS   /////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

const excerptTablist = document.querySelector('#lfe-excerpt-tabs')
if (excerptTablist) {
    const excerptTabs = Array.from(excerptTablist.querySelectorAll('[role="tab"]'))

    function activateExcerptTab(tab, focus = false) {
        excerptTabs.forEach((item) => {
            const selected = item === tab
            item.setAttribute('aria-selected', String(selected))
            item.tabIndex = selected ? 0 : -1
            item.classList.toggle('active', selected)

            const panel = document.getElementById(item.getAttribute('aria-controls'))
            panel?.classList.toggle('hidden', !selected)
        })
        if (focus) tab.focus()
    }

    excerptTablist.addEventListener('click', (event) => {
        const tab = event.target.closest('[role="tab"]')
        if (tab && excerptTablist.contains(tab)) activateExcerptTab(tab)
    })

    excerptTablist.addEventListener('keydown', (event) => {
        const tab = event.target.closest('[role="tab"]')
        if (!tab || !excerptTablist.contains(tab)) return

        const currentIndex = excerptTabs.indexOf(tab)
        let nextIndex
        if (event.key === 'ArrowRight') nextIndex = (currentIndex + 1) % excerptTabs.length
        else if (event.key === 'ArrowLeft') nextIndex = (currentIndex - 1 + excerptTabs.length) % excerptTabs.length
        else if (event.key === 'Home') nextIndex = 0
        else if (event.key === 'End') nextIndex = excerptTabs.length - 1
        else return

        event.preventDefault()
        activateExcerptTab(excerptTabs[nextIndex], true)
    })
}

