/** @type {import('tailwindcss').Config} */
module.exports = {
    darkMode: 'class',
    content: [
        "./_layouts/*.liquid",
        "./_layouts/**/*.liquid",
        "./_includes/*.liquid",
        "./_includes/**/*.liquid",
        "./styles/*.{html,js}",
        "./styles/**/*.{html,js}",
    ],
    theme: {
        extend: {},
    },
    plugins: [
        require('@tailwindcss/typography'),
    ],
}
