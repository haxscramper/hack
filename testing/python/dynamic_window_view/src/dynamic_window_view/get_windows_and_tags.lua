local function json_escape(s)
    s = tostring(s or "")
    s = s:gsub("\\", "\\\\")
    s = s:gsub('"', '\\"')
    s = s:gsub("\b", "\\b")
    s = s:gsub("\f", "\\f")
    s = s:gsub("\n", "\\n")
    s = s:gsub("\r", "\\r")
    s = s:gsub("\t", "\\t")
    return s
end

local function json_string(s)
    return '"' .. json_escape(s) .. '"'
end

local function json_number(n)
    return tostring(n)
end

local function json_boolean(v)
    return v and "true" or "false"
end

local function json_array(items)
    return "[" .. table.concat(items, ",") .. "]"
end

local function json_object(fields)
    return "{" .. table.concat(fields, ",") .. "}"
end

local function build_client_json(c)
    local client_tags = {}
    for _, tag in ipairs(c:tags()) do
        table.insert(client_tags, json_string(tag.name or ""))
    end

    local visible_on_selected_tag = false
    for _, tag in ipairs(c:tags()) do
        if tag.selected then
            visible_on_selected_tag = true
            break
        end
    end

    return json_object({
        '"wid":' .. json_number(c.window),
        '"title":' .. json_string(c.name or ""),
        '"wm_class":' .. json_string(c.class or ""),
        '"pid":' .. json_number(c.pid or -1),
        '"screen":' .. json_number(c.screen.index),
        '"x":' .. json_number(c.x),
        '"y":' .. json_number(c.y),
        '"width":' .. json_number(c.width),
        '"height":' .. json_number(c.height),
        '"floating":' .. json_boolean(c.floating),
        '"maximized":' .. json_boolean(c.maximized),
        '"minimized":' .. json_boolean(c.minimized),
        '"fullscreen":' .. json_boolean(c.fullscreen),
        '"urgent":' .. json_boolean(c.urgent),
        '"hidden":' .. json_boolean(c.hidden),
        '"ontop":' .. json_boolean(c.ontop),
        '"sticky":' .. json_boolean(c.sticky),
        '"above":' .. json_boolean(c.above),
        '"below":' .. json_boolean(c.below),
        '"visible_on_selected_tag":' .. json_boolean(visible_on_selected_tag),
        '"tags":' .. json_array(client_tags)
    })
end

local tags_json = {}

for s in screen do
    for _, tag in ipairs(s.tags) do
        local windows_json = {}
        for _, c in ipairs(tag:clients()) do
            table.insert(windows_json, build_client_json(c))
        end

        table.insert(tags_json, json_object({
            '"name":' .. json_string(tag.name or ""),
            '"screen":' .. json_number(s.index),
            '"selected":' .. json_boolean(tag.selected),
            '"activated":' .. json_boolean(tag.activated),
            '"index":' .. json_number(tag.index or -1),
            '"layout":' .. json_string(tag.layout and tag.layout.name or ""),
            '"master_count":' .. json_number(tag.master_count or 0),
            '"column_count":' .. json_number(tag.column_count or 0),
            '"master_width_factor":' .. tostring(tag.master_width_factor or 0),
            '"gap":' .. json_number(tag.gap or 0),
            '"volatile":' .. json_boolean(tag.volatile),
            '"windows":' .. json_array(windows_json)
        }))
    end
end

return json_array(tags_json)
