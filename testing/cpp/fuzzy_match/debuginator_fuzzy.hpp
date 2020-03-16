// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
// COPIED FROM https://github.com/Srekel/the-debuginator
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include <cstring>

int get_score(char current_full_path[128], char filter[32]) {
    bool taken_chars[128] = {0};
    // --- get new full path string --- //
    DEBUGINATOR_strcpy_s(
        current_full_path + path_indices[current_path_index],
        50,
        item->title);
    path_indices[current_path_index + 1] = path_indices[current_path_index]
                                           + (int)DEBUGINATOR_strlen(
                                               item->title);
    int current_path_length = path_indices[current_path_index + 1];

    // --- if not exact search do lowercase all --- //
    // if (!exact_search) {
    for (size_t i = path_indices[current_path_index];
         i < current_path_length;
         i++) {
        current_full_path[i] = (char)DEBUGINATOR_tolower(
            current_full_path[i]);
    }
    //}


    // --- item score --- //
    int  score       = -1;
    bool is_filtered = false;

    // LOL1010
    // 100
    int filter_part = 0;
    // --- iterate until the end of the filter --- //
    while (filter[filter_part] != '\0') {
        if (filter[filter_part] == ' ') {
            // --- skip whitespace --- //
            ++filter_part;
            continue;
        }

        int path_part   = 0;
        int matches[8]  = {0};
        int match_count = 0;
        // --- iterate over characters in full path --- //
        while (current_full_path[path_part] != '\0') {
            bool filter_part_found = false;
            for (int path_i = path_part; path_i < current_path_length;
                 path_i++) {
                if (current_full_path[path_i] == filter[filter_part]
                    && taken_chars[path_i] == false) {
                    path_part         = path_i;
                    filter_part_found = true;
                    break;
                }
            }

            if (!filter_part_found) {
                // match_count = 0;
                break;
            }

            int         match_length = 0;
            const char* filter_char  = filter + filter_part;
            const char* path_char    = current_full_path + path_part;
            while (*filter_char++ == *path_char++) {
                match_length++;
                if (*filter_char == '\0' || *filter_char == ' '
                    || taken_chars[path_part + match_length] == true) {
                    break;
                }
            }

            if (exact_search) {
                if (filter[filter_part + match_length] != '\0'
                    && filter[filter_part + match_length] != ' ') {
                    path_part += 1;
                    continue;
                }
            }

            matches[match_count++] = path_part;
            matches[match_count++] = match_length;
            path_part += match_length;

            if (match_count == 8) {
                break;
            }
        }

        int best_match_index = -1;
        int best_match_score = -1;
        for (int i = 0; i < match_count; i += 2) {
            int match_index         = matches[i];
            int match_length        = matches[i + 1];
            int is_word_break_start = match_index == 0
                                      || current_full_path[match_index - 1]
                                             == ' '
                                      || (!DEBUGINATOR_isalpha(
                                              current_full_path
                                                  [match_index - 1])
                                          && DEBUGINATOR_isalpha(
                                              current_full_path
                                                  [match_index]))
                                      || (!DEBUGINATOR_isdigit(
                                              current_full_path
                                                  [match_index - 1])
                                          && DEBUGINATOR_isdigit(
                                              current_full_path
                                                  [match_index]));
            int is_word_break_end = match_index + match_length
                                        == current_path_length
                                    || current_full_path
                                               [match_index + match_length]
                                           == ' '
                                    || (!DEBUGINATOR_isalpha(
                                            current_full_path
                                                [match_index
                                                 + match_length])
                                        && DEBUGINATOR_isalpha(
                                            current_full_path
                                                [match_index]))
                                    || (!DEBUGINATOR_isdigit(
                                            current_full_path
                                                [match_index
                                                 + match_length])
                                        && DEBUGINATOR_isdigit(
                                            current_full_path
                                                [match_index]));
            int is_match_in_item_title = match_index >= path_indices
                                             [current_path_index];
            int match_score = (is_word_break_start * 10
                               + is_word_break_end * 5
                               + is_match_in_item_title * 10
                               + match_length)
                              * match_length;
            if (match_score > best_match_score) {
                best_match_score = match_score;
                best_match_index = i;
            }
        }

        if (best_match_index == -1) {
            is_filtered = true;
            score       = -1;
            break; // Filter not valid
        } else {
            filter_part += matches[best_match_index + 1];
            score += best_match_score;
            // is_filtered = false;
            for (int match_i = 0; match_i < matches[best_match_index + 1];
                 match_i++) {
                taken_chars[matches[best_match_index] + match_i] = true;
            }
        }
    }

    if (is_filtered && !item->is_filtered) {
        debuginator__set_total_height(item, 0);
        debuginator__adjust_num_visible_children(item->parent, -1);
        item->leaf.is_expanded = false;
    } else if (!is_filtered && item->is_filtered) {
        debuginator__set_total_height(
            item, debuginator->item_height); // Hacky
        debuginator__adjust_num_visible_children(item->parent, 1);
    }

    item->is_filtered = is_filtered;

    return score;
}

void debuginator_update_filter(
    TheDebuginator* debuginator,
    const char*     wanted_filter) {
    // See this for a description of how the fuzzy filtering works.
    // https://medium.com/@Srekel/implementing-a-fuzzy-search-algorithm-for-the-debuginator-cacc349e6c55

    const int filter_length    = (int)strlen(wanted_filter);
    bool      expanding_search = false;
    if (filter_length < strlen(debuginator->filter)) {
        expanding_search = true;
        if (debuginator->hot_item->user_data == (void*)0x12345678) {
            debuginator_remove_item(debuginator, debuginator->hot_item);
        }
    } else if (filter_length > strlen(debuginator->filter)) {
        // TODO do memcmp here to check for completely new filter.
        if (debuginator->hot_item->user_data == (void*)0x12345678) {
            strcpy_s(
                debuginator->filter,
                sizeof(debuginator->filter),
                wanted_filter);
            return;
        }
    }

    // Exact search
    // "el eb" matches "Debuginator/Help"
    // "oo " doesn't match "lolol"
    // "aa aa" doesn't match "Cars/Saab and Volvo"
    // "aa aa" matches "Caars/Saab"
    bool exact_search = false;
    for (size_t i = 0; i < strlen(wanted_filter); i++) {
        if (wanted_filter[i] == ' ') {
            exact_search = true;
            break;
        }
    }

    // --- Convert filter to lowercase if case is not exact --- //
    char filter[32] = {0};
    // if (!exact_search) {
    for (size_t i = 0; i < 20; i++) {
        filter[i] = (char)tolower(wanted_filter[i]);
    }
    //}

    char current_full_path[128] = {0};
    int  path_indices[8]        = {0};
    int  current_path_index     = 0;

    int              best_score = -1;
    DebuginatorItem* best_item  = NULL;

    DebuginatorItem* item = debuginator->root->folder.first_child;
    // --- itertate over all items in tree --- //
    while (item != NULL) {
        if (item->is_folder) {
            if (item->folder.first_child != NULL) {
                DEBUGINATOR_strcpy_s(
                    current_full_path + path_indices[current_path_index],
                    sizeof(current_full_path)
                        - path_indices[current_path_index],
                    item->title);

                path_indices[current_path_index + 1] = path_indices
                                                           [current_path_index]
                                                       + (int)
                                                           DEBUGINATOR_strlen(
                                                               item->title);
                *(current_full_path + path_indices[current_path_index + 1]) = ' ';
                path_indices[current_path_index + 1]++;

                // if (!exact_search) {
                for (int i = path_indices[current_path_index];
                     i < path_indices[current_path_index + 1];
                     i++) {
                    current_full_path[i] = (char)DEBUGINATOR_tolower(
                        current_full_path[i]);
                }
                //}

                ++current_path_index;
                item = item->folder.first_child;

                continue;
            }
        } else {

            int score = get_score(current_full_path, filter);

            if (score > best_score) {
                if (item == debuginator->hot_item) {
                    score++;
                }
                best_score = score;
                best_item  = item;
            }
        }

        if (item->next_sibling != NULL) {
            item = item->next_sibling;
        } else {
            while (item->parent != NULL
                   && item->parent->next_sibling == NULL) {
                item = item->parent;
                --current_path_index;
            }

            if (item->parent == NULL) {
                // Went all the way 'back' to the menu root.
                break;
            }

            item = item->parent->next_sibling;
            --current_path_index;
        }
    }

    if (expanding_search && debuginator->hot_item != NULL) {
        // We're good. Just keep the previously hot item; it can't have
        // disappeared.
    } else if (best_item != NULL) {
        debuginator->hot_item               = best_item;
        best_item->parent->folder.hot_child = best_item;
    } else if (filter_length == 0) {
        // Happens when we remove the last letter of the search, and go
        // from only "No items found" to all items
        debuginator->hot_item = debuginator__find_first_leaf(
            debuginator->root);
    } else {
        DebuginatorItem* fallback = debuginator_create_array_item(
            debuginator,
            NULL,
            "No items found",
            "Your search filter returned no results.",
            NULL,
            (void*)0x12345678,
            NULL,
            NULL,
            0,
            0);
        debuginator->hot_item              = fallback;
        fallback->parent->folder.hot_child = fallback;
    }

    int distance_from_root_to_hot_item = 0;
    debuginator__distance_to_hot_item(
        debuginator->root,
        debuginator->hot_item,
        debuginator->item_height,
        &distance_from_root_to_hot_item);
    float wanted_y = debuginator->size.y * debuginator->focus_height;
    float distance_to_wanted_y = wanted_y - distance_from_root_to_hot_item;
    debuginator->current_height_offset = distance_to_wanted_y;

    DEBUGINATOR_strcpy_s(
        debuginator->filter, sizeof(debuginator->filter), filter);
}
