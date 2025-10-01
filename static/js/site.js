//////////////////////////////////////////////////////////////////////////////
///   DARK MODE   ////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

const HSThemeAppearance = {
    init() {
        const defaultTheme = 'default'
        let theme = localStorage.getItem('hs_theme') || defaultTheme

        if (document.querySelector('html').classList.contains('dark')) return
        this.setAppearance(theme)
    },
    _resetStylesOnLoad() {
        const $resetStyles = document.createElement('style')
        $resetStyles.innerText = `*{transition: unset !important;}`
        $resetStyles.setAttribute('data-hs-appearance-onload-styles', '')
        document.head.appendChild($resetStyles)
        return $resetStyles
    },
    setAppearance(theme, saveInStore = true, dispatchEvent = true) {
        const $resetStylesEl = this._resetStylesOnLoad()

        if (saveInStore) {
            localStorage.setItem('hs_theme', theme)
        }

        if (theme === 'auto') {
            theme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'default'
        }

        document.querySelector('html').classList.remove('dark')
        document.querySelector('html').classList.remove('default')
        document.querySelector('html').classList.remove('auto')

        document.querySelector('html').classList.add(this.getOriginalAppearance())

        setTimeout(() => {
            $resetStylesEl.remove()
        })

        if (dispatchEvent) {
            window.dispatchEvent(new CustomEvent('on-hs-appearance-change', {detail: theme}))
        }
    },
    getAppearance() {
        let theme = this.getOriginalAppearance()
        if (theme === 'auto') {
            theme = window.matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'default'
        }
        return theme
    },
    getOriginalAppearance() {
        const defaultTheme = 'default'
        return localStorage.getItem('hs_theme') || defaultTheme
    }
}
HSThemeAppearance.init()

window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
    if (HSThemeAppearance.getOriginalAppearance() === 'auto') {
        HSThemeAppearance.setAppearance('auto', false)
    }
})

window.addEventListener('load', () => {
    const $clickableThemes = document.querySelectorAll('[data-hs-theme-click-value]')
    const $switchableThemes = document.querySelectorAll('[data-hs-theme-switch]')

    $clickableThemes.forEach($item => {
        $item.addEventListener('click', () => HSThemeAppearance.setAppearance($item.getAttribute('data-hs-theme-click-value'), true, $item))
    })

    $switchableThemes.forEach($item => {
        $item.addEventListener('change', (e) => {
            HSThemeAppearance.setAppearance(e.target.checked ? 'dark' : 'default')
        })

        $item.checked = HSThemeAppearance.getAppearance() === 'dark'
    })

    window.addEventListener('on-hs-appearance-change', e => {
        $switchableThemes.forEach($item => {
            $item.checked = e.detail === 'dark'
        })
    })

    // Custom dropdown implementation (Preline has a bug with the close logic)
    const dropdown = document.querySelector('.custom-dropdown')
    const dropdownToggle = document.querySelector('.custom-dropdown-toggle')
    const dropdownMenu = document.querySelector('.custom-dropdown-menu')

    if (dropdown && dropdownToggle && dropdownMenu) {

        // Toggle dropdown on button click
        dropdownToggle.addEventListener('click', function(e) {
            e.preventDefault()
            e.stopPropagation()

            if (dropdown.classList.contains('open')) {
                closeDropdown()
            } else {
                openDropdown()
            }
        })

        // Close dropdown when clicking outside
        document.addEventListener('click', function(e) {
            // Don't close if clicking the toggle button (it handles itself)
            if (dropdownToggle.contains(e.target)) {
                return
            }

            // Close if clicking outside the dropdown
            if (!dropdown.contains(e.target)) {
                closeDropdown()
            }
        })

        // Close dropdown when clicking a menu item
        dropdownMenu.querySelectorAll('a').forEach(function(link) {
            link.addEventListener('click', function() {
                closeDropdown()
            })
        })

        function openDropdown() {
            dropdown.classList.add('open')

            // Calculate position for fixed positioning
            const rect = dropdownToggle.getBoundingClientRect()
            dropdownMenu.style.top = rect.bottom + 'px'
            dropdownMenu.style.left = rect.left + 'px'

            dropdownMenu.classList.remove('hidden')
            dropdownMenu.classList.add('block')
        }

        function closeDropdown() {
            dropdown.classList.remove('open')
            dropdownMenu.classList.remove('block')
            dropdownMenu.classList.add('hidden')
        }
    }
})

