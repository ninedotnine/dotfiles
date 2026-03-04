require('material').setup({
    contrast = {
        cursor_line = true,
    },
    styles = {
        comments = { italic = true },
        strings = { italic = true },
        variables = { bold = true },
        functions = { bold = true },
    },
    disable = {
        background = true, 
        colored_cursor = true, 
    },
    custom_highlights = {
--         CursorLine = { underline = true },
--
        TabLineSel = {
            bold = true,
            italic = false,
            underline = true,
        },

        -- haskell
        ConId = { 
            bold = true,
            italic = false,
            underline = false,
        },
        VarId = {
            bold = false,
            italic = false,
            underline = false,
        },
    },
})

-- vim.g.material_style = "oceanic"
-- vim.g.material_style = "deep ocean"
-- vim.g.material_style = "palenight"
-- vim.g.material_style = "darker"
vim.g.material_style = "lighter"
