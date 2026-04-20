/** @type {import('tailwindcss').Config} */
module.exports = {
    darkMode: 'class',
    content: [
        "./src/_layouts/*.liquid",
        "./src/_layouts/**/*.liquid",
        "./src/_includes/*.liquid",
        "./src/_includes/**/*.liquid",
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
