/* eslint-disable no-undef */
module.exports = {
    content: ["./src/App/*.purs", "./src/Main.purs", "./assets/*.html"],
    theme: {
        container: {
            center: true,
            padding: "2rem",
        },
        extend: {
            height: {
                "fit-content": "fit-content",
            },
        },
    },
    extend: {},

    darkMode: "media",

    plugins: [require("@tailwindcss/forms")],
}
