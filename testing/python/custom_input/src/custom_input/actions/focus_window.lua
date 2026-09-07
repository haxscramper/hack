local awful = require("awful")

local function find_client(window_id)
    for _, current_client in ipairs(client.get()) do
        if current_client.window == window_id then
            return current_client
        end
    end

    return nil
end

local function capture_screen_state()
    local state = {}

    for current_screen in screen do
        local selected_tags = {}

        for _, current_tag in ipairs(current_screen.selected_tags) do
            table.insert(selected_tags, current_tag)
        end

        state[current_screen] = {
            tags = selected_tags,
            focused_client = awful.client.focus.history.get(
                current_screen,
                0
            ),
        }
    end

    return state
end

local function restore_screen_state(current_screen, state)
    if #state.tags > 0 then
        local selected_tags = {}

        for _, current_tag in ipairs(state.tags) do
            if current_tag.screen == current_screen then
                selected_tags[current_tag] = true
            end
        end

        state.tags[1].selected = true

        for _, current_tag in ipairs(current_screen.tags) do
            current_tag.selected = selected_tags[current_tag] == true
        end
    end

    if state.focused_client ~= nil and state.focused_client.valid then
        state.focused_client:raise()
    end
end

return function(window_id, reset_state)
    local target = find_client(window_id)

    if target == nil then
        return false
    end

    if reset_state or _G.custom_input_focus_state == nil then
        _G.custom_input_focus_state = capture_screen_state()
    else
        for current_screen, state in pairs(
            _G.custom_input_focus_state
        ) do
            if current_screen ~= target.screen then
                restore_screen_state(current_screen, state)
            end
        end
    end

    target:jump_to()
    target:raise()

    return true
end
