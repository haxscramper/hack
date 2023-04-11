#include <string>
#include <vector>
#include <utility>
#include <memory>
#include <functional>
#include <ranges>
#include <optional>
#include <cmath>
#include <iostream>

#include <cassert>

#include <fmt/core.h>

class Source;

// A trait implemented by `Source` caches.
using Id = int;

// A trait implemented by spans within a character-based source.
class Span {
  public:
    // Get the identifier of the source that this span refers to.
    virtual const Id source() const = 0;

    // Get the start offset of this span.
    // Offsets are zero-indexed character offsets from the beginning of the
    // source.
    virtual std::size_t start() const = 0;

    // Get the (exclusive) end offset of this span.
    // The end offset should *always* be greater than or equal to the start
    // offset as given by Span::start(). Offsets are zero-indexed character
    // offsets from the beginning of the source.
    virtual std::size_t end() const = 0;

    // Get the length of this span (difference between the start of the
    // span and the end of the span).
    std::size_t len() const { return end() - start(); }

    // Determine whether the span contains the given offset.
    bool contains(std::size_t offset) const { return start() <= offset && offset < end(); }
};

// Span implementation for Range<usize>
class RangeSpan : public Span {
  public:
    explicit RangeSpan(std::pair<size_t, size_t> range) : range_(range) {}

    virtual const Id source() const override { return -1; }
    std::size_t      start() const override { return range_.first; }
    std::size_t      end() const override { return range_.second; }

  private:
    std::pair<size_t, size_t> range_;
};

// Span implementation for (Id, Range<usize>)
class TupleSpan : public Span {
  public:
    explicit TupleSpan(Id id, std::pair<size_t, size_t> range) : id_(std::move(id)), range_(range) {}

    const Id    source() const override { return id_; }
    std::size_t start() const override { return range_.first; }
    std::size_t end() const override { return range_.second; }

  private:
    Id                        id_;
    std::pair<size_t, size_t> range_;
};

class Cache {
  public:
    // Fetch the `Source` identified by the given ID, if possible.
    // TODO: Don't box
    virtual std::shared_ptr<Source> fetch(Id const& id) = 0;

    // Display the given ID. as a single inline value.
    virtual std::optional<std::string> display(Id const& id) const = 0;
};

// A type representing a single line of a `Source`.
struct Line {
    size_t      offset;
    size_t      len;
    std::string chars;

    // Get the offset of this line in the original `Source` (i.e: the
    // number of characters that precede it).
    size_t get_offset() const { return offset; }

    // Get the character length of this line.
    size_t get_len() const { return len; }

    // Get the offset span of this line in the original `Source`.
    std::pair<size_t, size_t> span() const { return {offset, offset + len}; }

    // Return an iterator over the characters in the line, excluding
    // trailing whitespace.
    // TODO: Implement the iterator for `Line` class
};

// A type representing a single source that may be referred to by `Span`s.
//
// In most cases, a source is a single input file.
struct Source {
    std::vector<Line> lines;
    size_t            len;

    struct OffsetLine {
        const Line& line;
        size_t      idx = 0;
        size_t      col = 0;
    };

    // Get the line that the given offset appears on, and the line/column
    // numbers of the offset. Note that the line/column numbers are
    // zero-indexed.
    std::optional<OffsetLine> get_offset_line(size_t offset) {
        if (offset <= len) {
            auto it = std::lower_bound(lines.begin(), lines.end(), offset, [](const Line& line, size_t offset) {
                return line.offset < offset;
            });
            if (it != lines.begin()) {
                --it;
            }
            size_t      idx  = std::distance(lines.begin(), it);
            const Line& line = lines[idx];
            assert(offset >= line.offset);
            return OffsetLine{std::ref(line), idx, offset - line.offset};
        } else {
            return std::nullopt;
        }
    }

    // Get the range of lines that this span runs across.
    // The resulting range is guaranteed to contain valid line indices
    // (i.e: those that can be used for Source::line()).
    template <typename S>
    std::pair<size_t, size_t> get_line_range(const S& span) {
        size_t start = get_offset_line(span.start())
                           .value_or(std::make_tuple(std::ref(lines[0]), 0, 0))
                           .template get<1>();
        size_t end = get_offset_line(std::max(span.end() - 1, span.start()))
                         .value_or(std::make_tuple(std::ref(lines[lines.size() - 1]), lines.size() - 1, 0))
                         .template get<1>()
                   + 1;
        return std::make_pair(start, end);
    }
};

struct Characters {
    wchar_t hbar;
    wchar_t vbar;
    wchar_t xbar;
    wchar_t vbar_break;
    wchar_t vbar_gap;

    wchar_t uarrow;
    wchar_t rarrow;

    wchar_t ltop;
    wchar_t mtop;
    wchar_t rtop;
    wchar_t lbot;
    wchar_t rbot;
    wchar_t mbot;

    wchar_t lbox;
    wchar_t rbox;

    wchar_t lcross;
    wchar_t rcross;

    wchar_t underbar;
    wchar_t underline;
};

Characters unicode() {
    return Characters{
        .hbar       = L'─',
        .vbar       = L'│',
        .xbar       = L'┼',
        .vbar_break = L'┆',
        .vbar_gap   = L'┆',
        .uarrow     = L'🭯',
        .rarrow     = L'▶',
        .ltop       = L'╭',
        .mtop       = L'┬',
        .rtop       = L'╮',
        .lbot       = L'╰',
        .rbot       = L'╯',
        .mbot       = L'┴',
        .lbox       = L'[',
        .rbox       = L']',
        .lcross     = L'├',
        .rcross     = L'┤',
        .underbar   = L'┬',
        .underline  = L'─',
    };
}

Characters ascii() {
    return Characters{
        .hbar       = '-',
        .vbar       = '|',
        .xbar       = '+',
        .vbar_break = '*',
        .vbar_gap   = ':',
        .uarrow     = '^',
        .rarrow     = '>',
        .ltop       = ',',
        .mtop       = 'v',
        .rtop       = '.',
        .lbot       = '`',
        .rbot       = '\'',
        .mbot       = '^',
        .lbox       = '[',
        .rbox       = ']',
        .lcross     = '|',
        .rcross     = '|',
        .underbar   = '|',
        .underline  = '^',
    };
}

// use std::borrow::Borrow;
// use std::io;
// use std::ops::Range;

// use super::draw::{self, StreamAwareFmt, StreamType};
// use super::{Cache, CharSet, Label, LabelAttach, Report, ReportKind,
// Show, Span, Write};

// // A WARNING, FOR ALL YE WHO VENTURE IN HERE
// //
// // - This code is complex and has a lot of implicit invariants
// // - Yes, it has some bugs
// // - Yes, it needs rewriting
// // - No, you are not expected to understand it. I will probably not
// understand it either in a month, but that will only
// //   give me a reason to rewrite it

// enum LabelKind {
//     Inline,
//     Multiline,
// }

enum LabelKind
{
    Inline,
    Multiline
};

enum class Color
{
    Unset,
    Default,
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    Fixed,
    RGB,
};

struct Label {
    // Create a new Label
    static Label newLabel(const std::shared_ptr<Span>& span) { return Label(span); }

    // Give this label a message
    Label with_message(const std::string& msg) {
        this->msg = msg;
        return *this;
    }

    // Give this label a highlight color
    Label with_color(const Color& color) {
        this->color = color;
        return *this;
    }

    // Specify the order of this label relative to other labels
    Label with_order(int32_t order) {
        this->order = order;
        return *this;
    }

    // Specify the priority of this label relative to other labels
    Label with_priority(int32_t priority) {
        this->priority = priority;
        return *this;
    }

    // Private constructor
    Label(const std::shared_ptr<Span>& span) : span(span), msg(""), color(Color()), order(0), priority(0) {}

    std::shared_ptr<Span> span;
    std::string           msg;
    Color                 color;
    int32_t               order;
    int32_t               priority;
};


// struct LabelInfo<'a, S> {
//     kind: LabelKind,
//     label: &'a Label<S>,
// }

struct LabelInfo {
    LabelKind   kind;
    const Label label;
};

// struct SourceGroup<'a, S: Span> {
//     src_id: &'a S::SourceId,
//     span: Range<usize>,
//     labels: Vec<LabelInfo<'a, S>>,
// }

struct SourceGroup {
    Id                                  src_id;
    std::pair<std::size_t, std::size_t> span; // Replacing Range<usize>
                                              // with a pair of size_t
    std::vector<LabelInfo> labels;
};

enum class ReportKind
{
    Error,
    Warning,
    Advice,
    Custom
};

enum class LabelAttach
{
    // Arrows should attach to the start of the label span.
    Start,
    // Arrows should attach to the middle of the label span (or as close to
    // the middle as we can get).
    Middle,
    // Arrows should attach to the end of the label span.
    End,
};


enum class CharSet
{
    // Unicode characters (an attempt is made to use only
    // commonly-supported characters).
    Unicode,
    // ASCII-only characters.
    Ascii,
};

struct Config {
    Config()
        : cross_gap(true)
        , label_attach(LabelAttach::Middle)
        , compact(false)
        , underlines(true)
        , multiline_arrows(true)
        , color(true)
        , tab_width(4)
        , char_set(CharSet::Unicode) {}

    Config& with_cross_gap(bool cross_gap) {
        this->cross_gap = cross_gap;
        return *this;
    }

    Config& with_label_attach(LabelAttach label_attach) {
        this->label_attach = label_attach;
        return *this;
    }

    Config& with_compact(bool compact) {
        this->compact = compact;
        return *this;
    }

    Config& with_underlines(bool underlines) {
        this->underlines = underlines;
        return *this;
    }

    Config& with_multiline_arrows(bool multiline_arrows) {
        this->multiline_arrows = multiline_arrows;
        return *this;
    }

    Config& with_color(bool color) {
        this->color = color;
        return *this;
    }

    Config& with_tab_width(size_t tab_width) {
        this->tab_width = tab_width;
        return *this;
    }

    Config& with_char_set(CharSet char_set) {
        this->char_set = char_set;
        return *this;
    }

    std::optional<Color> error_color() { return color ? std::make_optional(Color::Red) : std::nullopt; }

    std::optional<Color> warning_color() { return color ? std::make_optional(Color::Yellow) : std::nullopt; }

    std::optional<Color> advice_color() { return color ? std::make_optional(Color::Fixed) : std::nullopt; }

    std::optional<Color> margin_color() { return color ? std::make_optional(Color::Fixed) : std::nullopt; }

    std::optional<Color> unimportant_color() { return color ? std::make_optional(Color::Fixed) : std::nullopt; }

    std::optional<Color> note_color() { return color ? std::make_optional(Color::Fixed) : std::nullopt; }

    bool        cross_gap;
    LabelAttach label_attach;
    bool        compact;
    bool        underlines;
    bool        multiline_arrows;
    bool        color;
    size_t      tab_width;
    CharSet     char_set;
};

class Report {
  public:
    ReportKind                 kind;
    std::optional<std::string> code;
    std::optional<std::string> msg;
    std::optional<std::string> note;
    std::optional<std::string> help;
    std::pair<Id, std::size_t> location;
    std::vector<Label>         labels;
    Config                     config;

    // fn get_source_groups(&self, cache: &mut impl Cache<S::SourceId>) ->
    // Vec<SourceGroup<S>> {
    //     let mut groups = Vec::new();
    //     for label in self.labels.iter() {
    //         let src_display = cache.display(label.span.source());
    //         let src = match cache.fetch(label.span.source()) {
    //             Ok(src) => src,
    //             Err(e) => {
    //                 eprintln!("Unable to fetch source '{}': {:?}",
    //                 Show(src_display), e); continue;
    //             }
    //         };

    //         assert!(
    //             label.span.start() <= label.span.end(),
    //             "Label start is after its end"
    //         );

    //         let start_line =
    //         src.get_offset_line(label.span.start()).map(|(_, l, _)| l);
    //         let end_line = src
    //             .get_offset_line(label.span.end().saturating_sub(1).max(label.span.start()))
    //             .map(|(_, l, _)| l);

    //         let label_info = LabelInfo {
    //             kind: if start_line == end_line {
    //                 LabelKind::Inline
    //             } else {
    //                 LabelKind::Multiline
    //             },
    //             label,
    //         };

    //         if let Some(group) = groups
    //             .iter_mut()
    //             .find(|g: &&mut SourceGroup<S>| g.src_id ==
    //             label.span.source())
    //         {
    //             group.span.start =
    //             group.span.start.min(label.span.start()); group.span.end
    //             = group.span.end.max(label.span.end());
    //             group.labels.push(label_info);
    //         } else {
    //             groups.push(SourceGroup {
    //                 src_id: label.span.source(),
    //                 span: label.span.start()..label.span.end(),
    //                 labels: vec![label_info],
    //             });
    //         }
    //     }
    //     groups
    // }

    std::vector<SourceGroup> get_source_groups(Cache* cache) {
        std::vector<SourceGroup> groups;
        for (const auto& label : labels) {
            auto                    src_display = cache->display(label.span->source());
            std::shared_ptr<Source> src         = cache->fetch(label.span->source());
            if (!src) {
                continue;
            }

            assert(label.span->start() <= label.span->end());

            // "Label start is after its end");

            auto start_line = src->get_offset_line(label.span->start()).value().idx;

            auto end_line = src->get_offset_line(std::max(label.span->end() - 1, label.span->start())).value().idx;

            LabelInfo label_info{
                .kind = (start_line == end_line) ? LabelKind::Inline : LabelKind::Multiline, .label = label};

            auto group_it = std::find_if(groups.begin(), groups.end(), [&](const SourceGroup& group) {
                return group.src_id == label.span->source();
            });

            if (group_it != groups.end()) {
                group_it->span.first  = std::min(group_it->span.first, label.span->start());
                group_it->span.second = std::max(group_it->span.second, label.span->end());
                group_it->labels.push_back(label_info);
            } else {
                groups.push_back(SourceGroup{
                    .src_id = label.span->source(),
                    .span   = std::make_pair(label.span->start(), label.span->end()),
                    .labels = {label_info}});
            }
        }
        return groups;
    }


    std::error_code write(Cache cache, std::ostream w) { return write_for_stream(cache, w); }

    std::error_code write_for_stream(Cache& cache, std::ostream& s) {
        Characters draw;
        switch (config.char_set) {
            case CharSet::Unicode: draw = unicode(); break;
            case CharSet::Ascii: draw = ascii(); break;
        }

        // --- Header ---
        std::optional<std::string> code;
        if (code.has_value()) {
            code = "[" + *code + "] ";
        }
        std::string id = "TODO_FORMAT"; // fmt::format("{}{}:", code,
                                        // kind);
        std::optional<Color> kind_color;
        switch (kind) {
            case ReportKind::Error: kind_color = config.error_color(); break;
            case ReportKind::Warning: kind_color = config.warning_color(); break;
            case ReportKind::Advice: kind_color = config.advice_color(); break;
            case ReportKind::Custom: kind_color = config.unimportant_color(); break;
        }

        //        fmt::print(w, "{} {}", id.fg(kind_color, s),
        //        Show(msg.as_ref()));

        auto groups = get_source_groups(&cache);

        // Line number maximum width
        int line_no_width = 0;
        for (const auto& group : groups) {
            std::string src_name = cache.display(group.src_id).value_or("<unknown>");

            try {
                auto src = cache.fetch(group.src_id);

                auto line_range = src->get_line_range(group.span);
                int  width      = 0;
                for (uint32_t x = 1, y = 1; line_range.second / y != 0; x *= 10, y = std::pow(10, x)) {
                    ++width;
                }
                line_no_width = std::max(line_no_width, width);
            } catch (const std::exception& e) {
                std::cerr << "Unable to fetch source " << src_name << ": " << e.what() << std::endl;
            }
        }

        // --- Source sections ---
        for (int group_idx = 0; group_idx < groups.size(); ++group_idx) {
            SourceGroup const& group           = groups[group_idx];
            auto const& [src_id, span, labels] = group;

            std::string src_name = cache.display(src_id).value_or("<unknown>");

            std::shared_ptr<Source> src;
            try {
                src = cache.fetch(src_id);
            } catch (const std::exception& e) {
                std::cerr << "Unable to fetch source " << src_name << ": " << e.what() << std::endl;
                continue;
            }

            auto line_range = src->get_line_range(span);

            // File name & reference
            size_t location = (src_id == this->location.first) ? this->location.second : labels[0].label.span->start();

            auto                                offset_line = src->get_offset_line(location);
            std::pair<std::string, std::string> line_col;
            if (offset_line) {
                line_col = {fmt::format("{}", offset_line->idx + 1), fmt::format("{}", offset_line->col + 1)};
            } else {
                line_col = {"?", "?"};
            }

            if (!config.compact) {}
        }


        struct LineLabel {
            size_t       col;
            const Label& label;
            bool         multi;
            bool         draw_msg;
        };

        // Generate a list of multi-line labels
        std::vector<const Label*> multi_labels;
        for (const auto& label_info : labels) {
            if (label_info.kind == LabelKind::Multiline) {
                multi_labels.push_back(&label_info.label);
            }
        }

        // Sort multiline labels by length
        std::sort(multi_labels.begin(), multi_labels.end(), [](const Label<S>* a, const Label<S>* b) {
            return (a->span.len()) > (b->span.len());
        });


        auto write_margin = [&](std::ostream&                          w,
                                size_t                                 idx,
                                bool                                   is_line,
                                bool                                   is_ellipsis,
                                bool                                   draw_labels,
                                std::optional<std::pair<size_t, bool>> report_row,
                                const std::vector<LineLabel>&          line_labels,
                                const std::optional<LineLabel>&        margin_label) {
            if (draw_labels) {
                for (size_t col = 0; col < multi_labels.size() + (multi_labels.size() > 0); ++col) {
                    std::optional<std::pair<const Label*, bool>>     corner     = std::nullopt;
                    std::optional<const Label*>                      hbar       = std::nullopt;
                    std::optional<const Label*>                      vbar       = std::nullopt;
                    std::optional<std::pair<const LineLabel*, bool>> margin_ptr = std::nullopt;

                    const Label* multi_label = (col < multi_label->size()) ? &multi_label[col] : nullptr;
                    auto         line_span   = src.line(idx)->span();

                    for (size_t i = 0; i < std::min(col + 1, multi_labels.size()); ++i) {
                        const auto& label  = multi_label[i];
                        auto        margin = margin_label ? (label == margin_label->label ? &(*margin_label) : nullptr)
                                                          : nullptr;

                        if (label.span->start() <= line_span.end && label.span->end() > line_span.start) {
                            bool is_parent = i != col;
                            bool is_start  = line_span.contains(label.span->start());
                            bool is_end    = line_span.contains(label.last_offset());

                            if (margin && is_line) {
                                margin_ptr = std::make_pair(margin, is_start);
                            } else if (!is_start && (!is_end || is_line)) {
                                vbar = vbar ? vbar : (!is_parent ? &label : nullptr);
                            } else if (report_row.has_value()) {
                                auto   report_row_value = report_row.value();
                                size_t label_row        = 0;
                                for (size_t r = 0; r < line_labels.size(); ++r) {
                                    if (label == line_labels[r].label) {
                                        label_row = r;
                                        break;
                                    }
                                }

                                if (report_row_value.first == label_row) {
                                    if (margin) {
                                        vbar = (col == i) ? &margin->label : nullptr;
                                        if (is_start) {
                                            continue;
                                        }
                                    }

                                    if (report_row_value.second) {
                                        hbar = &label;
                                        if (!is_parent) {
                                            corner = std::make_pair(&label, is_start);
                                        }
                                    } else if (!is_start) {
                                        vbar = vbar ? vbar : (!is_parent ? &label : nullptr);
                                    }
                                } else {
                                    vbar = vbar ? vbar
                                                : (!is_parent && (is_start ^ (report_row_value.first < label_row))
                                                       ? &label
                                                       : nullptr);
                                }
                            }
                        }
                    }
                }
            }
        };


        is_ellipsis = false;
            for{
                idx in line_range {
                    let line = if let Some(line) = src.line(idx) { line }
                    else {
                        continue;
                    };


                    std::vector<LineLabel> line_labels;

                    for (const auto& label : multi_labels) {
                        bool is_start = line.span().contains(label->span.start());
                        bool is_end   = line.span().contains(label->last_offset());
                        if (is_start && (margin_label == std::nullopt || *label != margin_label->label)) {
                            line_labels.push_back(
                                LineLabel{static_cast<int>(label->span.start() - line.offset()), *label, true, false});
                        } else if (is_end) {
                            line_labels.push_back(
                                LineLabel{static_cast<int>(label->last_offset() - line.offset()), *label, true, true});
                        }
                    }

                    for (const auto& label_info : labels) {
                        if (label_info.label.span.start() >= line.span().start
                            && label_info.label.span.end() <= line.span().end && label_info.kind == LabelKind::Inline) {
                            int col;
                            switch (self.config.label_attach) {
                                case LabelAttach::Start: col = label_info.label.span.start(); break;
                                case LabelAttach::Middle:
                                    col = (label_info.label.span.start() + label_info.label.span.end()) / 2;
                                    break;
                                case LabelAttach::End: col = label_info.label.last_offset(); break;
                            }
                            col = std::max(col, static_cast<int>(label_info.label.span.start())) - line.offset();
                            line_labels.push_back(LineLabel{col, label_info.label, false, true});
                        }
                    }


                    if (line_labels.empty() && margin_label == std::nullopt) {
                        bool within_label = std::any_of(
                            multi_labels.begin(), multi_labels.end(), [&](const auto& label) {
                                return label->span.contains(line.span().start());
                            });
                        if (!is_ellipsis && within_label) {
                            is_ellipsis = true;
                        } else {
                            if (!self.config.compact && !is_ellipsis) {
                                write_margin(w, idx, false, is_ellipsis, false, std::nullopt, {}, std::nullopt);
                                w << "\n";
                            }
                            is_ellipsis = true;
                            continue;
                        }
                    } else {
                        is_ellipsis = false;
                    }


                    // Sort the labels by their columns
                    std::sort(line_labels.begin(), line_labels.end(), [](const auto& ll1, const auto& ll2) {
                        return std::tie(ll1.label.order, ll1.col, !ll1.label.span.start())
                             < std::tie(ll2.label.order, ll2.col, !ll2.label.span.start());
                    });

                    // Determine label bounds so we know where to put error
                    // messages let arrow_end_space = if
                    int arrow_end_space = self.config.compact ? 1 : 2;
                    int arrow_len       = std::accumulate(
                                        line_labels.begin(),
                                        line_labels.end(),
                                        0,
                                        [&](int l, const auto& ll) {
                                            return ll.multi
                                                           ? line.length()
                                                           : std::max(
                                                         l, static_cast<int>(ll.label.span.end() - line.offset()));
                                        })
                                  + arrow_end_space;


                    // Should we draw a vertical bar as part of a label
                    // arrow on this line?
                    auto get_vbar = [&](int col, int row) {
                        auto it = std::find_if(line_labels.begin(), line_labels.end(), [&](const auto& ll) {
                            return ll.label.msg.has_value()
                                && (margin_label == std::nullopt || ll.label != margin_label->label) && ll.col == col
                                && ((row <= &ll - &line_labels[0] && !ll.multi)
                                    || (row <= &ll - &line_labels[0] && ll.multi));
                        });
                        return it != line_labels.end() ? &*it : nullptr;
                    };

                    auto get_highlight = [&](int col) {
                        auto it = std::min_element(
                            margin_label.begin(), margin_label.end(), [&](const auto& l1, const auto& l2) {
                                return std::tie(-l1.priority, l1.span.length())
                                     < std::tie(-l2.priority, l2.span.length());
                            });
                        return it != margin_label.end() && it->span.contains(line.offset() + col) ? &*it : nullptr;
                    };


                    auto get_underline = [&](int col) {
                        auto it = std::min_element(
                            line_labels.begin(), line_labels.end(), [&](const auto& ll1, const auto& ll2) {
                                return std::tie(-ll1.label.priority, ll1.label.span.length())
                                     < std::tie(-ll2.label.priority, ll2.label.span.length());
                            });
                        return it != line_labels.end() && self.config.underlines && !it->multi
                                    && it->label.span.contains(line.offset() + col)
                                 ? &*it
                                 : nullptr;
                    };

                    write_margin(w, idx, true, is_ellipsis, true, std::nullopt, line_labels, margin_label);

                    // Line
                    if (!is_ellipsis) {
                        int col = 0;
                        for (char c : line) {
                            auto highlight = get_highlight(col);
                            auto color     = highlight.has_value() ? highlight->color : self.config.unimportant_color();
                            auto [c_, width] = self.config.char_width(c, col);
                            if (isspace(c_)) {
                                for (int i = 0; i < width; ++i) {
                                    w << fg(c_, color, s);
                                }
                            } else {
                                w << fg(c_, color, s);
                            }
                            ++col;
                        }
                    }
                    w << "\n";


                    // Arrows
                for{
                    row in 0..line_labels.len() {
                        let line_label = &line_labels[row];

                        if (!self.config.compact) {
                            // Margin alternate
                            write_margin(
                                w,
                                idx,
                                false,
                                is_ellipsis,
                                true,
                                std::make_pair(row, false),
                                line_labels,
                                margin_label);
                            // Lines alternate
                            auto chars = line.begin();
                            for (size_t col = 0; col < arrow_len; ++col) {
                                int width = chars == line.end() ? 1 : self.config.char_width(*chars, col).second;

                                auto vbar      = get_vbar(col, row);
                                auto underline = get_underline(col).value_or(false);
                                auto [c, tail] = vbar.has_value() ? ([&] {
                                    auto vbar_ll = vbar.value();
                                    if (underline) {
                                        // TODO: Is this good?
                                        if (vbar_ll.label.span.length() <= 1 || true) {
                                            return std::make_pair(draw.underbar, draw.underline);
                                        } else if (line.offset() + col == vbar_ll.label.span.start()) {
                                            return std::make_pair(draw.ltop, draw.underbar);
                                        } else if (line.offset() + col == vbar_ll.label.last_offset()) {
                                            return std::make_pair(draw.rtop, draw.underbar);
                                        } else {
                                            return std::make_pair(draw.underbar, draw.underline);
                                        }
                                    } else if (vbar_ll.multi && row == 0 && self.config.multiline_arrows) {
                                        return std::make_pair(draw.uarrow, ' ');
                                    } else {
                                        return std::make_pair(draw.vbar, ' ');
                                    }
                                }())
                                                                  : (underline ? std::make_pair(
                                                                         draw.underline, draw.underline)
                                                                               : std::make_pair(' ', ' '));
                                c              = c.fg(vbar.value().label.color, s);
                                tail           = tail.fg(vbar.value().label.color, s);

                                for (int i = 0; i < width; ++i) {
                                    w << (i == 0 ? c : tail);
                                }
                            }
                            w << "\n";
                        }


                        write_margin(
                            w, idx, false, is_ellipsis, true, std::make_pair(row, true), line_labels, margin_label);

                        auto chars = line.begin();
                        for (size_t col = 0; col < arrow_len; ++col) {
                            int width = chars == line.end() ? 1 : self.config.char_width(*chars, col).second;

                            bool is_hbar = (((col > line_label.col) ^ line_label.multi)
                                            || (line_label.label.msg.has_value() && line_label.draw_msg
                                                && col > line_label.col))
                                        && line_label.label.msg.has_value();
                            auto [c, tail] = (col == line_label.col && line_label.label.msg.has_value()
                                              && (!margin_label.has_value()
                                                  || line_label.label != margin_label.value().label))
                                               ? (std::make_pair(
                                                   (line_label.multi ? (line_label.draw_msg ? draw.mbot : draw.rbot)
                                                                     : draw.lbot)
                                                       .fg(line_label.label.color, s),
                                                   draw.hbar.fg(line_label.label.color, s)))
                                               : ([&]() {
                                                     auto vbar_ll = get_vbar(col, row);
                                                     if (vbar_ll.has_value()
                                                         && (col != line_label.col
                                                             || line_label.label.msg.has_value())) {
                                                         if (!self.config.cross_gap && is_hbar) {
                                                             return std::make_pair(
                                                                 draw.xbar.fg(line_label.label.color, s),
                                                                 ' '.fg(line_label.label.color, s));
                                                         } else if (is_hbar) {
                                                             return std::make_pair(
                                                                 draw.hbar.fg(line_label.label.color, s),
                                                                 draw.hbar.fg(line_label.label.color, s));
                                                         } else {
                                                             return std::make_pair(
                                                                 (vbar_ll.value().multi && row == 0
                                                                          && self.config.compact
                                                                      ? draw.uarrow
                                                                      : draw.vbar)
                                                                     .fg(vbar_ll.value().label.color, s),
                                                                 ' '.fg(line_label.label.color, s));
                                                         }
                                                     } else if (is_hbar) {
                                                         return std::make_pair(
                                                             draw.hbar.fg(line_label.label.color, s),
                                                             draw.hbar.fg(line_label.label.color, s));
                                                     } else {
                                                         return std::make_pair(
                                                             ' '.fg(std::nullopt, s), ' '.fg(std::nullopt, s));
                                                     }
                                                 }());

                            if (width > 0) {
                                w << c;
                            }
                            for (int i = 1; i < width; ++i) {
                                w << tail;
                            }
                        }


                        if line_label {
                            .draw_msg { write !(w, " {}", Show(line_label.label.msg.as_ref())) ? ; }
                        }
                        write !(w, "\n") ? ;
                    }
                }
                }
            }

            // let is_final_group = group_idx + 1 == groups_len;
            bool is_final_group = group_idx + 1 == groups_len;

            // Help
            if (self.help.has_value() && is_final_group) {
                if (!self.config.compact) {
                write_margin(w, 0, false, false, true, std::make_pair(0, false), std::vector<Label>(), std::nullopt);
                w << "\n";
                }
                write_margin(w, 0, false, false, true, std::make_pair(0, false), std::vector<Label>(), std::nullopt);
                w << "Help".fg(self.config.note_color(), s) << ": " << self.help.value() << "\n";
            }


            // Note
            if (self.note.has_value() && is_final_group) {
                if (!self.config.compact) {
                write_margin(w, 0, false, false, true, std::make_pair(0, false), std::vector<Label>(), std::nullopt);
                w << "\n";
                }
                write_margin(w, 0, false, false, true, std::make_pair(0, false), std::vector<Label>(), std::nullopt);
                w << "Note".fg(self.config.note_color(), s) << ": " << self.note.value() << "\n";
            }

            // Tail of report
            if (!self.config.compact) {
                if (is_final_group) {
                std::string final_margin = format("{}{}", Show(draw.hbar, line_no_width + 2), draw.rbot);
                w << final_margin.fg(self.config.margin_color(), s) << std::endl;
                } else {
                w << Show(' ', line_no_width + 2) << draw.vbar.fg(self.config.margin_color(), s) << std::endl;
                }
            }
    }
}
}
}
;
