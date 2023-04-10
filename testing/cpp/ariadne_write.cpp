#include <string>
#include <vector>
#include <utility>
#include <memory>
#include <functional>
#include <ranges>
#include <optional>

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
    bool contains(std::size_t offset) const {
        return start() <= offset && offset < end();
    }
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
    explicit TupleSpan(Id id, std::pair<size_t, size_t> range)
        : id_(std::move(id)), range_(range) {}

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
    virtual Source* fetch(Id const& id) = 0;

    // Display the given ID. as a single inline value.
    //
    // This function may make use of attributes from the `Fmt` trait.
    // TODO: Don't box
    virtual std::unique_ptr<std::function<void(std::ostream&)>> display(Id const& id) const = 0;
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
    std::pair<size_t, size_t> span() const {
        return {offset, offset + len};
    }

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

    // Get the line that the given offset appears on, and the line/column
    // numbers of the offset. Note that the line/column numbers are
    // zero-indexed.
    std::optional<std::tuple<const Line&, size_t, size_t>> get_offset_line(
        size_t offset) {
        if (offset <= len) {
            auto it = std::lower_bound(
                lines.begin(),
                lines.end(),
                offset,
                [](const Line& line, size_t offset) {
                    return line.offset < offset;
                });
            if (it != lines.begin()) {
                --it;
            }
            size_t      idx  = std::distance(lines.begin(), it);
            const Line& line = lines[idx];
            assert(offset >= line.offset);
            return std::make_tuple(
                std::ref(line), idx, offset - line.offset);
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
                           .value_or(
                               std::make_tuple(std::ref(lines[0]), 0, 0))
                           .template get<1>();
        size_t end = get_offset_line(
                         std::max(span.end() - 1, span.start()))
                         .value_or(std::make_tuple(
                             std::ref(lines[lines.size() - 1]),
                             lines.size() - 1,
                             0))
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
enum class Color {
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
    static Label newLabel(const std::shared_ptr<Span>& span) {
        return Label(span);
    }

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
    Label(const std::shared_ptr<Span>& span)
        : span(span), msg(""), color(Color()), order(0), priority(0) {}

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

template <typename S>
struct SourceGroup {
    const typename S::SourceId*         src_id;
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

    std::optional<Color> error_color() {
        return color ? std::make_optional(Color::Red) : std::nullopt;
    }

    std::optional<Color> warning_color() {
        return color ? std::make_optional(Color::Yellow) : std::nullopt;
    }

    std::optional<Color> advice_color() {
        return color ? std::make_optional(Color::Fixed) : std::nullopt;
    }

    std::optional<Color> margin_color() {
        return color ? std::make_optional(Color::Fixed) : std::nullopt;
    }

    std::optional<Color> unimportant_color() {
        return color ? std::make_optional(Color::Fixed) : std::nullopt;
    }

    std::optional<Color> note_color() {
        return color ? std::make_optional(Color::Fixed) : std::nullopt;
    }

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

    template <typename S>
    std::vector<SourceGroup<S>> get_source_groups(Cache* cache) {
        std::vector<SourceGroup<S>> groups;
        for (const auto& label : labels) {
            auto    src_display = cache->display(label.span->source());
            Source* src         = cache->fetch(label.span->source());
            if (!src) {
                continue;
            }

            assert(label.span->start() <= label.span->end());

            // "Label start is after its end");

            auto start_line = std::get<1>(
                src->get_offset_line(label.span->start()).value());

            auto end_line = std::get<1>(
                src
                    ->get_offset_line(std::max(
                        label.span->end() - 1, label.span->start()))
                    .value());

            LabelInfo label_info{
                .kind  = (start_line == end_line) ? LabelKind::Inline
                                                  : LabelKind::Multiline,
                .label = label};

            auto group_it = std::find_if(
                groups.begin(),
                groups.end(),
                [&](const SourceGroup<S>& group) {
                    return group.src_id == label.span->source();
                });

            if (group_it != groups.end()) {
                group_it->span.first = std::min(
                    group_it->span.first, label.span->start());
                group_it->span.second = std::max(
                    group_it->span.second, label.span->end());
                group_it->labels.push_back(label_info);
            } else {
                groups.push_back(SourceGroup<S>{
                    .src_id = label.span->source(),
                    .span   = std::make_pair(
                        label.span->start(), label.span->end()),
                    .labels = {label_info}});
            }
        }
        return groups;
    }


    std::error_code write(Cache cache, std::ostream w) {
        return write_for_stream(cache, w);
    }

    std::error_code write_for_stream(Cache& cache, std::ostream& s) {

        //         let draw = match self.config.char_set {
        //             CharSet::Unicode => draw::Characters::unicode(),
        //             CharSet::Ascii => draw::Characters::ascii(),
        //         };

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
            case ReportKind::Error:
                kind_color = config.error_color();
                break;
            case ReportKind::Warning:
                kind_color = config.warning_color();
                break;
            case ReportKind::Advice:
                kind_color = config.advice_color();
                break;
            case ReportKind::Custom: kind_color = std::get<1>(kind); break;
        }

        fmt::print(w, "{} {}", id.fg(kind_color, s), Show(msg.as_ref()));

        auto groups = get_source_groups(&cache);

        // Line number maximum width
        int line_no_width = 0;
        for (const auto& group : groups) {
            std::string src_name = cache.display(group.src_id)
                                       .value_or("<unknown>");

            try {
                auto src = cache.fetch(group.src_id);

                auto line_range = src.get_line_range(group.span);
                int  width      = 0;
                for (uint32_t x = 1, y = 1; line_range.end / y != 0;
                     x *= 10, y        = 10u32_pow(x)) {
                    ++width;
                }
                line_no_width = std::max(line_no_width, width);
            } catch (const std::exception& e) {
                std::cerr << "Unable to fetch source " << src_name << ": "
                          << e.what() << std::endl;
            }
        }


        //         // --- Header ---

        //         let code = self.code.as_ref().map(|c| format!("[{}] ",
        //         c)); let id = format!("{}{}:", Show(code), self.kind);
        //         let kind_color = match self.kind {
        //             ReportKind::Error => self.config.error_color(),
        //             ReportKind::Warning => self.config.warning_color(),
        //             ReportKind::Advice => self.config.advice_color(),
        //             ReportKind::Custom(_, color) => Some(color),
        //         };
        //         writeln!(w, "{} {}", id.fg(kind_color, s),
        //         Show(self.msg.as_ref()))?;

        //         let groups = self.get_source_groups(&mut cache);

        //         // Line number maximum width
        //         let line_no_width = groups
        //             .iter()
        //             .filter_map(|SourceGroup { span, src_id, .. }| {
        //                 let src_name = cache
        //                     .display(src_id)
        //                     .map(|d| d.to_string())
        //                     .unwrap_or_else(|| "<unknown>".to_string());

        //                 let src = match cache.fetch(src_id) {
        //                     Ok(src) => src,
        //                     Err(e) => {
        //                         eprintln!("Unable to fetch source {}:
        //                         {:?}", src_name, e); return None;
        //                     }
        //                 };

        //                 let line_range = src.get_line_range(span);
        //                 Some(
        //                     (1..)
        //                         .map(|x| 10u32.pow(x))
        //                         .take_while(|x| line_range.end as u32 /
        //                         x != 0) .count()
        //                         + 1,
        //                 )
        //             })
        //             .max()
        //             .unwrap_or(0);

        //         // --- Source sections ---
        //         let groups_len = groups.len();
        //         for (
        //             group_idx,
        //             SourceGroup {
        //                 src_id,
        //                 span,
        //                 labels,
        //             },
        //         ) in groups.into_iter().enumerate()
        //         {
        //             let src_name = cache
        //                 .display(src_id)
        //                 .map(|d| d.to_string())
        //                 .unwrap_or_else(|| "<unknown>".to_string());

        //             let src = match cache.fetch(src_id) {
        //                 Ok(src) => src,
        //                 Err(e) => {
        //                     eprintln!("Unable to fetch source {}: {:?}",
        //                     src_name, e); continue;
        //                 }
        //             };

        //             let line_range = src.get_line_range(&span);

        //             // File name & reference
        //             let location = if src_id == self.location.0.borrow()
        //             {
        //                 self.location.1
        //             } else {
        //                 labels[0].label.span.start()
        //             };
        //             let (line_no, col_no) = src
        //                 .get_offset_line(location)
        //                 .map(|(_, idx, col)| (format!("{}", idx + 1),
        //                 format!("{}", col + 1))) .unwrap_or_else(||
        //                 ('?'.to_string(), '?'.to_string()));
        //             let line_ref = format!(":{}:{}", line_no, col_no);
        //             writeln!(
        //                 w,
        //                 "{}{}{}{}{}{}{}",
        //                 Show((' ', line_no_width + 2)),
        //                 if group_idx == 0 {
        //                     draw.ltop
        //                 } else {
        //                     draw.lcross
        //                 }
        //                 .fg(self.config.margin_color(), s),
        //                 draw.hbar.fg(self.config.margin_color(), s),
        //                 draw.lbox.fg(self.config.margin_color(), s),
        //                 src_name,
        //                 line_ref,
        //                 draw.rbox.fg(self.config.margin_color(), s),
        //             )?;

        //             if !self.config.compact {
        //                 writeln!(
        //                     w,
        //                     "{}{}",
        //                     Show((' ', line_no_width + 2)),
        //                     draw.vbar.fg(self.config.margin_color(), s)
        //                 )?;
        //             }

        //             struct LineLabel<'a, S> {
        //                 col: usize,
        //                 label: &'a Label<S>,
        //                 multi: bool,
        //                 draw_msg: bool,
        //             }

        std::string src_name = cache.display(src_id).value_or("<unknown>");

        std::shared_ptr<Source> src;
        try {
            src = cache.fetch(src_id);
        } catch (const std::exception& e) {
            std::cerr << "Unable to fetch source " << src_name << ": "
                      << e.what() << std::endl;
            continue;
        }

        auto line_range = src->get_line_range(span);

        // File name & reference
        size_t location = src_id == *self.location.first.lock()
                            ? self.location.second
                            : labels[0].label.span.start();
        std::pair<std::string, std::string> line_col;
        try {
            auto offset_line = src->get_offset_line(location);
            line_col         = {
                fmt::format("{}", offset_line.idx + 1),
                fmt::format("{}", offset_line.col + 1)};
        } catch (...) { line_col = {"?", "?"}; }
        std::string line_ref = fmt::format(
            ":{}:{}", line_col.first, line_col.second);
        fmt::print(
            w,
            "{}{}{}{}{}{}{}",
            Show(' ', line_no_width + 2),
            group_idx == 0 ? draw.ltop
                           : draw.lcross.fg(self.config.margin_color(), s),
            draw.hbar.fg(self.config.margin_color(), s),
            draw.lbox.fg(self.config.margin_color(), s),
            src_name,
            line_ref,
            draw.rbox.fg(self.config.margin_color(), s));

        if (!self.config.compact) {
            fmt::print(
                w,
                "{}{}",
                Show(' ', line_no_width + 2),
                draw.vbar.fg(self.config.margin_color(), s));
        }

        struct LineLabel {
            size_t       col;
            const Label& label;
            bool         multi;
            bool         draw_msg;
        };


        //             // Generate a list of multi-line labels
        //             let mut multi_labels = Vec::new();
        //             for label_info in &labels {
        //                 if matches!(label_info.kind,
        //                 LabelKind::Multiline) {
        //                     multi_labels.push(&label_info.label);
        //                 }
        //             }

        //             // Sort multiline labels by length
        //             multi_labels.sort_by_key(|m| -(m.span.len() as
        //             isize));

        //             let write_margin = |w: &mut W,
        //                                 idx: usize,
        //                                 is_line: bool,
        //                                 is_ellipsis: bool,
        //                                 draw_labels: bool,
        //                                 report_row: Option<(usize,
        //                                 bool)>, line_labels:
        //                                 &[LineLabel<S>], margin_label:
        //                                 &Option<LineLabel<S>>|
        //              -> std::io::Result<()> {
        // let line_no_margin = if is_line && !is_ellipsis {
        //     let line_no = format!("{}", idx + 1);
        //     format!(
        //         "{}{} {}",
        //         Show((' ', line_no_width - line_no.chars().count())),
        //         line_no,
        //         draw.vbar,
        //     )
        //     .fg(self.config.margin_color(), s)
        // } else {
        //     format!(
        //         "{}{}",
        //         Show((' ', line_no_width + 1)),
        //         if is_ellipsis {
        //             draw.vbar_gap
        //         } else {
        //             draw.vbar
        //         }
        //     )
        //     .fg(self.config.skipped_margin_color(), s)
        // };

        // write!(
        //     w,
        //     " {}{}",
        //     line_no_margin,
        //     Show(Some(' ').filter(|_| !self.config.compact)),
        // )?;

        std::string line_no_margin;
        if (is_line && !is_ellipsis) {
            std::string line_no      = fmt::format("{}", idx + 1);
            size_t      margin_space = line_no_width - line_no.length();
            line_no_margin           = fmt::format(
                                 "{}{} {}",
                                 Show(' ', margin_space),
                                 line_no,
                                 draw.vbar)
                                 .fg(self.config.margin_color(), s);
        } else {
            std::string bar = is_ellipsis ? draw.vbar_gap : draw.vbar;
            line_no_margin  = fmt::format(
                                 "{}{}", Show(' ', line_no_width + 1), bar)
                                 .fg(self.config.skipped_margin_color(),
                                     s);
        }

        fmt::print(
            w,
            " {}{}",
            line_no_margin,
            Show(
                self.config.compact ? std::nullopt
                                    : std::optional<char>(' ')));


        if (draw_labels) {
            for (size_t col = 0;
                 col < multi_labels.size() + (multi_labels.size() > 0);
                 ++col) {

                //                 // Multi-line margins
                //                 if draw_labels {
                //                     for col in 0..multi_labels.len() +
                //                     (multi_labels.len() > 0) as usize {
                // let mut corner = None;
                // let mut hbar = None;
                // let mut vbar: Option<&&Label<S>> = None;
                // let mut margin_ptr = None;

                std::optional<Label<S>*>                        corner;
                std::optional<Label<S>*>                        hbar;
                std::optional<Label<S>*>                        vbar;
                std::optional<std::pair<MarginLabel<S>*, bool>> margin_ptr;
                // let multi_label = multi_labels.get(col);
                // let line_span = src.line(idx).unwrap().span();
                auto multi_label = multi_labels.find(col);
                auto line_span   = src.line(idx).value().span();


                // for (i, label) in multi_labels[0..(col +
                // 1).min(multi_labels.len())]
                //     .iter()
                //     .enumerate()
                // {
                for (size_t i = 0;
                     i < std::min(col + 1, multi_labels.size());
                     ++i) {
                    auto label = multi_labels[i];
                    //     let margin = margin_label
                    //         .as_ref()
                    //         .filter(|m| **label as *const _ == m.label
                    //         as *const _);
                    auto margin = margin_label.has_value()
                                       && (label == margin_label->label)
                                    ? margin_label
                                    : std::nullopt;
                    //     if label.span.start() <= line_span.end
                    //         && label.span.end() > line_span.start
                    //     {
                    //         let is_parent = i != col;
                    //         let is_start =
                    //         line_span.contains(&label.span.start()); let
                    //         is_end =
                    //         line_span.contains(&label.last_offset());
                    if (label->span.start() <= line_span.second
                        && label->span.end() > line_span.first) {
                        bool is_parent = i != col;
                        bool is_start  = line_span.first
                                         <= label->span.start()
                                     && label->span.start()
                                            < line_span.second;
                        bool is_end = line_span.first
                                       <= label->last_offset()
                                   && label->last_offset()
                                          < line_span.second;

                        //         if let Some(margin) = margin.filter(|_|
                        //         is_line) {
                        //             margin_ptr = Some((margin,
                        //             is_start));
                        //         } else if !is_start && (!is_end ||
                        //         is_line) {
                        //             vbar =
                        //             vbar.or(Some(*label).filter(|_|
                        //             !is_parent));
                        //         } else if let Some((report_row,
                        //         is_arrow)) = report_row {
                        //             let label_row = line_labels
                        //                 .iter()
                        //                 .enumerate()
                        //                 .find(|(_, l)| **label as *const
                        //                 _ == l.label as *const _)
                        //                 .map_or(0, |(r, _)| r);
                        //             if report_row == label_row {
                        //                 if let Some(margin) = margin {
                        //                     vbar =
                        //                     Some(&margin.label).filter(|_|
                        //                     col == i); if is_start {
                        //                         continue;
                        //                     }
                        //                 }

                        //                 if is_arrow {
                        //                     hbar = Some(**label);
                        //                     if !is_parent {
                        //                         corner = Some((label,
                        //                         is_start));
                        //                     }
                        //                 } else if !is_start {
                        //                     vbar =
                        //                     vbar.or(Some(*label).filter(|_|
                        //                     !is_parent));
                        //                 }
                        //             } else {
                        //                 vbar =
                        //                 vbar.or(Some(*label).filter(|_|
                        //                 {
                        //                     !is_parent && (is_start ^
                        //                     (report_row < label_row))
                        //                 }));
                        //             }
                        //         }
                        if (margin.has_value() && is_line) {
                            margin_ptr.emplace(margin.value(), is_start);
                        } else if (!is_start && (!is_end || is_line)) {
                            vbar = vbar.value_or(
                                !is_parent ? label : nullptr);
                        } else if (report_row.has_value()) {
                            auto label_row_it = std::find_if(
                                line_labels.begin(),
                                line_labels.end(),
                                [label](auto const& l) {
                                    return label == l.label;
                                });
                            size_t label_row = (label_row_it
                                                != line_labels.end())
                                                 ? std::distance(
                                                     line_labels.begin(),
                                                     label_row_it)
                                                 : 0;

                            if (report_row.value() == label_row) {
                                if (margin.has_value()) {
                                    vbar = col == i ? &margin->label
                                                    : nullptr;
                                    if (is_start) {
                                        continue;
                                    }
                                }

                                if (is_arrow) {
                                    hbar = label;
                                    if (!is_parent) {
                                        corner.emplace(label, is_start);
                                    }
                                } else if (!is_start) {
                                    vbar = vbar.value_or(
                                        !is_parent ? label : nullptr);
                                }
                            } else {
                                vbar = vbar.value_or(
                                    !is_parent
                                            && (is_start
                                                ^ (report_row.value()
                                                   < label_row))
                                        ? label
                                        : nullptr);
                            }
                        }
                        //     }
                        // }
                    }
                }


                // if let (Some((margin, _is_start)), true) = (margin_ptr,
                // is_line) {
                //     let is_col = multi_label
                //         .map_or(false, |ml| **ml as *const _ ==
                //         margin.label as *const _);
                //     let is_limit = col + 1 == multi_labels.len();
                //     if !is_col && !is_limit {
                //         hbar = hbar.or(Some(margin.label));
                //     }
                // }
                if (margin_ptr.has_value() && is_line) {
                    bool is_col = multi_label.has_value()
                               && (multi_label->get()
                                   == margin_ptr->first->label);
                    bool is_limit = col + 1 == multi_labels.size();
                    if (!is_col && !is_limit) {
                        hbar = hbar.value_or(margin_ptr->first->label);
                    }
                }
                // hbar = hbar.filter(|l| {
                //     margin_label
                //         .as_ref()
                //         .map_or(true, |margin| margin.label as *const _
                //         != *l as *const _)
                //         || !is_line
                // });

                // let (a, b) = if let Some((label, is_start)) = corner {
                //     (
                //         if is_start { draw.ltop } else { draw.lbot
                //         }.fg(label.color, s), draw.hbar.fg(label.color,
                //         s),
                //     )
                // } else if let Some(label) =
                //     hbar.filter(|_| vbar.is_some() &&
                //     !self.config.cross_gap)
                // {
                //     (draw.xbar.fg(label.color, s),
                //     draw.hbar.fg(label.color, s))
                // } else if let Some(label) = hbar {
                //     (draw.hbar.fg(label.color, s),
                //     draw.hbar.fg(label.color, s))
                // } else if let Some(label) = vbar {
                //     (
                //         if is_ellipsis {
                //             draw.vbar_gap
                //         } else {
                //             draw.vbar
                //         }
                //         .fg(label.color, s),
                //         ' '.fg(None, s),
                //     )
                // } else if let (Some((margin, is_start)), true) =
                // (margin_ptr, is_line) {
                //     let is_col = multi_label
                //         .map_or(false, |ml| **ml as *const _ ==
                //         margin.label as *const _);
                //     let is_limit = col == multi_labels.len();
                //     (
                //         if is_limit {
                //             draw.rarrow
                //         } else if is_col {
                //             if is_start {
                //                 draw.ltop
                //             } else {
                //                 draw.lcross
                //             }
                //         } else {
                //             draw.hbar
                //         }
                //         .fg(margin.label.color, s),
                //         if !is_limit { draw.hbar } else { ' '
                //         }.fg(margin.label.color, s),
                //     )
                // } else {
                //     (' '.fg(None, s), ' '.fg(None, s))
                // };
                hbar = hbar.has_value()
                            && (margin_label.has_value()
                                    && margin_label->label != hbar.value()
                                || !is_line)
                         ? hbar
                         : std::nullopt;

                std::pair<char, std::optional<Color>> a, b;
                if (corner.has_value()) {
                    auto label    = corner->first;
                    bool is_start = corner->second;
                    a = {is_start ? draw.ltop : draw.lbot, label->color};
                    b = {draw.hbar, label->color};
                } else if (
                    hbar.has_value() && vbar.has_value()
                    && !config.cross_gap) {
                    auto label = hbar.value();
                    a          = {draw.xbar, label->color};
                    b          = {draw.hbar, label->color};
                } else if (hbar.has_value()) {
                    auto label = hbar.value();
                    a          = {draw.hbar, label->color};
                    b          = {draw.hbar, label->color};
                } else if (vbar.has_value()) {
                    auto label = vbar.value();
                    a          = {
                        is_ellipsis ? draw.vbar_gap : draw.vbar,
                        label->color};
                    b = {' ', std::nullopt};
                } else if (margin_ptr.has_value() && is_line) {
                    auto margin   = margin_ptr->first;
                    bool is_start = margin_ptr->second;
                    bool is_col   = multi_label.has_value()
                               && (multi_label->get() == margin->label);
                    bool is_limit = col == multi_labels.size();
                    a             = {
                        is_limit
                                        ? draw.rarrow
                                        : (is_col
                                               ? (is_start ? draw.ltop : draw.lcross)
                                               : draw.hbar),
                        margin->label->color};
                    b = {
                        !is_limit ? draw.hbar : ' ', margin->label->color};
                } else {
                    a = {' ', std::nullopt};
                    b = {' ', std::nullopt};
                }
                // write!(w, "{}", a)?;
                // if !self.config.compact {
                //     write!(w, "{}", b)?;
                // }
                w << a.first;
                if (!config.compact) {
                    w << b.first;
                }


                //                     }
                //                 }

                //                 Ok(())
                //             };

                let mut is_ellipsis = false;
            for{
                idx in line_range {
                    let line = if let Some(line) = src.line(idx) { line }
                    else {
                        continue;
                    };

                    let margin_label = multi_labels
                    .iter()
                    .enumerate()
                    .filter_map(|(_i, label)| {
                        let is_start = line.span().contains(&label.span.start());
                        let is_end = line.span().contains(&label.last_offset());
                        if is_start {
                            // TODO: Check to see whether multi is the first on the start line or first on the end line
                            Some(LineLabel {
                                col: label.span.start() - line.offset(),
                                label: **label,
                                multi: true,
                                draw_msg: false, // Multi-line spans don;t have their messages drawn at the start
                            })
                        } else if is_end {
                            Some(LineLabel {
                                col: label.last_offset() - line.offset(),
                                label: **label,
                                multi: true,
                                draw_msg: true, // Multi-line spans have their messages drawn at the end
                            })
                        } else {
                            None
                        }
                    })
                    .min_by_key(|ll| (ll.col, !ll.label.span.start()));

                    // Generate a list of labels for this line, along with
                    // their label columns let mut line_labels =
                    // multi_labels
                    //     .iter()
                    //     .enumerate()
                    //     .filter_map(|(_i, label)| {
                    //         let is_start =
                    //         line.span().contains(&label.span.start());
                    //         let is_end =
                    //         line.span().contains(&label.last_offset());
                    //         if is_start
                    //             && margin_label
                    //                 .as_ref()
                    //                 .map_or(true, |m| **label as *const
                    //                 _ != m.label as *const _)
                    //         {
                    //             // TODO: Check to see whether multi is
                    //             the first on the start line or first on
                    //             the end line Some(LineLabel {
                    //                 col: label.span.start() -
                    //                 line.offset(), label: **label,
                    //                 multi: true,
                    //                 draw_msg: false, // Multi-line spans
                    //                 don;t have their messages drawn at
                    //                 the start
                    //             })
                    //         } else if is_end {
                    //             Some(LineLabel {
                    //                 col: label.last_offset() -
                    //                 line.offset(), label: **label,
                    //                 multi: true,
                    //                 draw_msg: true, // Multi-line spans
                    //                 have their messages drawn at the end
                    //             })
                    //         } else {
                    //             None
                    //         }
                    //     })
                    //     .collect::<Vec<_>>();

                    std::vector<LineLabel> line_labels;

                    for (const auto& label : multi_labels) {
                        bool is_start = line.span().contains(
                            label->span.start());
                        bool is_end = line.span().contains(
                            label->last_offset());
                        if (is_start
                            && (margin_label == std::nullopt
                                || *label != margin_label->label)) {
                            line_labels.push_back(LineLabel{
                                static_cast<int>(
                                    label->span.start() - line.offset()),
                                *label,
                                true,
                                false});
                        } else if (is_end) {
                            line_labels.push_back(LineLabel{
                                static_cast<int>(
                                    label->last_offset() - line.offset()),
                                *label,
                                true,
                                true});
                        }
                    }


                    // for label_info in labels.iter().filter(|l| {
                    //     l.label.span.start() >= line.span().start
                    //         && l.label.span.end() <= line.span().end
                    // }) {
                    //     if matches!(label_info.kind, LabelKind::Inline)
                    //     {
                    //         line_labels.push(LineLabel {
                    //             col: match &self.config.label_attach {
                    //                 LabelAttach::Start =>
                    //                 label_info.label.span.start(),
                    //                 LabelAttach::Middle => {
                    //                     (label_info.label.span.start() +
                    //                     label_info.label.span.end())
                    //                         / 2
                    //                 }
                    //                 LabelAttach::End =>
                    //                 label_info.label.last_offset(),
                    //             }
                    //             .max(label_info.label.span.start())
                    //                 - line.offset(),
                    //             label: label_info.label,
                    //             multi: false,
                    //             draw_msg: true,
                    //         });
                    //     }
                    // }

                    for (const auto& label_info : labels) {
                        if (label_info.label.span.start()
                                >= line.span().start
                            && label_info.label.span.end()
                                   <= line.span().end
                            && label_info.kind == LabelKind::Inline) {
                            int col;
                            switch (self.config.label_attach) {
                                case LabelAttach::Start:
                                    col = label_info.label.span.start();
                                    break;
                                case LabelAttach::Middle:
                                    col = (label_info.label.span.start()
                                           + label_info.label.span.end())
                                        / 2;
                                    break;
                                case LabelAttach::End:
                                    col = label_info.label.last_offset();
                                    break;
                            }
                            col = std::max(
                                      col,
                                      static_cast<int>(
                                          label_info.label.span.start()))
                                - line.offset();
                            line_labels.push_back(LineLabel{
                                col, label_info.label, false, true});
                        }
                    }

                    // Skip this line if we don't have labels for it
                    // if line_labels.len() == 0 && margin_label.is_none()
                    // {
                    //     let within_label = multi_labels
                    //         .iter()
                    //         .any(|label|
                    //         label.span.contains(line.span().start()));
                    //     if !is_ellipsis && within_label {
                    //         is_ellipsis = true;
                    //     } else {
                    //         if !self.config.compact && !is_ellipsis {
                    //             write_margin(&mut w, idx, false,
                    //             is_ellipsis, false, None, &[], &None)?;
                    //             write!(w, "\n")?;
                    //         }
                    //         is_ellipsis = true;
                    //         continue;
                    //     }
                    // } else {
                    //     is_ellipsis = false;
                    // }
                    if (line_labels.empty()
                        && margin_label == std::nullopt) {
                        bool within_label = std::any_of(
                            multi_labels.begin(),
                            multi_labels.end(),
                            [&](const auto& label) {
                                return label->span.contains(
                                    line.span().start());
                            });
                        if (!is_ellipsis && within_label) {
                            is_ellipsis = true;
                        } else {
                            if (!self.config.compact && !is_ellipsis) {
                                write_margin(
                                    w,
                                    idx,
                                    false,
                                    is_ellipsis,
                                    false,
                                    std::nullopt,
                                    {},
                                    std::nullopt);
                                w << "\n";
                            }
                            is_ellipsis = true;
                            continue;
                        }
                    } else {
                        is_ellipsis = false;
                    }


                    // Sort the labels by their columns
                    // line_labels.sort_by_key(|ll| (ll.label.order,
                    // ll.col, !ll.label.span.start()));
                    std::sort(
                        line_labels.begin(),
                        line_labels.end(),
                        [](const auto& ll1, const auto& ll2) {
                            return std::tie(
                                       ll1.label.order,
                                       ll1.col,
                                       !ll1.label.span.start())
                                 < std::tie(
                                       ll2.label.order,
                                       ll2.col,
                                       !ll2.label.span.start());
                        });

                    // Determine label bounds so we know where to put error
                    // messages let arrow_end_space = if
                    // self.config.compact { 1 } else { 2 }; let arrow_len
                    // = line_labels.iter().fold(0, |l, ll| {
                    //     if ll.multi {
                    //         line.len()
                    //     } else {
                    //         l.max(ll.label.span.end().saturating_sub(line.offset()))
                    //     }
                    // }) + arrow_end_space;
                    int arrow_end_space = self.config.compact ? 1 : 2;
                    int arrow_len       = std::accumulate(
                                        line_labels.begin(),
                                        line_labels.end(),
                                        0,
                                        [&](int l, const auto& ll) {
                                            return ll.multi
                                                           ? line.length()
                                                           : std::max(
                                                         l,
                                                         static_cast<int>(
                                                             ll.label.span
                                                                 .end()
                                                             - line.offset()));
                                        })
                                  + arrow_end_space;


                    // Should we draw a vertical bar as part of a label
                    // arrow on this line? let get_vbar = |col, row| {
                    //     line_labels
                    //         .iter()
                    //         // Only labels with notes get an arrow
                    //         .enumerate()
                    //         .filter(|(_, ll)| {
                    //             ll.label.msg.is_some()
                    //                 && margin_label
                    //                     .as_ref()
                    //                     .map_or(true, |m| ll.label as
                    //                     *const _ != m.label as *const _)
                    //         })
                    //         .find(|(j, ll)| {
                    //             ll.col == col && ((row <= *j &&
                    //             !ll.multi) || (row <= *j && ll.multi))
                    //         })
                    //         .map(|(_, ll)| ll)
                    // };

                    auto get_vbar = [&](int col, int row) {
                        auto it = std::find_if(
                            line_labels.begin(),
                            line_labels.end(),
                            [&](const auto& ll) {
                                return ll.label.msg.has_value()
                                    && (margin_label == std::nullopt
                                        || ll.label != margin_label->label)
                                    && ll.col == col
                                    && ((row <= &ll - &line_labels[0]
                                         && !ll.multi)
                                        || (row <= &ll - &line_labels[0]
                                            && ll.multi));
                            });
                        return it != line_labels.end() ? &*it : nullptr;
                    };


                    // let get_highlight = |col| {
                    //     margin_label
                    //         .iter()
                    //         .map(|ll| ll.label)
                    //         .chain(multi_labels.iter().map(|l| **l))
                    //         .chain(line_labels.iter().map(|l| l.label))
                    //         .filter(|l| l.span.contains(line.offset() +
                    //         col))
                    //         // Prioritise displaying smaller spans
                    //         .min_by_key(|l| (-l.priority, l.span.len()))
                    // };

                    auto get_highlight = [&](int col) {
                        auto it = std::min_element(
                            margin_label.begin(),
                            margin_label.end(),
                            [&](const auto& l1, const auto& l2) {
                                return std::tie(
                                           -l1.priority, l1.span.length())
                                     < std::tie(
                                           -l2.priority, l2.span.length());
                            });
                        return it != margin_label.end()
                                    && it->span.contains(
                                        line.offset() + col)
                                 ? &*it
                                 : nullptr;
                    };

                    // let get_underline = |col| {
                    //     line_labels
                    //         .iter()
                    //         .filter(|ll| {
                    //             self.config.underlines
                    //         // Underlines only occur for inline spans
                    //         (highlighting can occur for all spans)
                    //         && !ll.multi
                    //         && ll.label.span.contains(line.offset() +
                    //         col)
                    //         })
                    //         // Prioritise displaying smaller spans
                    //         .min_by_key(|ll| (-ll.label.priority,
                    //         ll.label.span.len()))
                    // };

                    auto get_underline = [&](int col) {
                        auto it = std::min_element(
                            line_labels.begin(),
                            line_labels.end(),
                            [&](const auto& ll1, const auto& ll2) {
                                return std::tie(
                                           -ll1.label.priority,
                                           ll1.label.span.length())
                                     < std::tie(
                                           -ll2.label.priority,
                                           ll2.label.span.length());
                            });
                        return it != line_labels.end()
                                    && self.config.underlines && !it->multi
                                    && it->label.span.contains(
                                        line.offset() + col)
                                 ? &*it
                                 : nullptr;
                    };


                    // Margin
                    // write_margin(
                    //     &mut w,
                    //     idx,
                    //     true,
                    //     is_ellipsis,
                    //     true,
                    //     None,
                    //     &line_labels,
                    //     &margin_label,
                    // )?;
                    write_margin(
                        w,
                        idx,
                        true,
                        is_ellipsis,
                        true,
                        std::nullopt,
                        line_labels,
                        margin_label);

                    // Line
                    // if !is_ellipsis {
                    //     for (col, c) in line.chars().enumerate() {
                    //         let color = if let Some(highlight) =
                    //         get_highlight(col) {
                    //             highlight.color
                    //         } else {
                    //             self.config.unimportant_color()
                    //         };
                    //         let (c, width) = self.config.char_width(c,
                    //         col); if c.is_whitespace() {
                    //             for _ in 0..width {
                    //                 write!(w, "{}", c.fg(color, s))?;
                    //             }
                    //         } else {
                    //             write!(w, "{}", c.fg(color, s))?;
                    //         };
                    //     }
                    // }
                    // write!(w, "\n")?;
                    if (!is_ellipsis) {
                        int col = 0;
                        for (char c : line) {
                            auto highlight   = get_highlight(col);
                            auto color       = highlight.has_value()
                                                 ? highlight->color
                                                 : self.config
                                                 .unimportant_color();
                            auto [c_, width] = self.config.char_width(
                                c, col);
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

                        // if !self.config.compact {
                        //     // Margin alternate
                        //     write_margin(
                        //         &mut w,
                        //         idx,
                        //         false,
                        //         is_ellipsis,
                        //         true,
                        //         Some((row, false)),
                        //         &line_labels,
                        //         &margin_label,
                        //     )?;
                        //     // Lines alternate
                        //     let mut chars = line.chars();
                        //     for col in 0..arrow_len {
                        //         let width =
                        //             chars.next().map_or(1, |c|
                        //             self.config.char_width(c, col).1);

                        //         let vbar = get_vbar(col, row);
                        //         let underline =
                        //         get_underline(col).filter(|_| row == 0);
                        //         let [c, tail] = if let Some(vbar_ll) =
                        //         vbar {
                        //             let [c, tail] = if
                        //             underline.is_some() {
                        //                 // TODO: Is this good?
                        //                 if vbar_ll.label.span.len() <= 1
                        //                 || true {
                        //                     [draw.underbar,
                        //                     draw.underline]
                        //                 } else if line.offset() + col ==
                        //                 vbar_ll.label.span.start() {
                        //                     [draw.ltop, draw.underbar]
                        //                 } else if line.offset() + col ==
                        //                 vbar_ll.label.last_offset() {
                        //                     [draw.rtop, draw.underbar]
                        //                 } else {
                        //                     [draw.underbar,
                        //                     draw.underline]
                        //                 }
                        //             } else if vbar_ll.multi && row == 0
                        //             && self.config.multiline_arrows
                        //             {
                        //                 [draw.uarrow, ' ']
                        //             } else {
                        //                 [draw.vbar, ' ']
                        //             };
                        //             [
                        //                 c.fg(vbar_ll.label.color, s),
                        //                 tail.fg(vbar_ll.label.color, s),
                        //             ]
                        //         } else if let Some(underline_ll) =
                        //         underline {
                        //             [draw.underline.fg(underline_ll.label.color,
                        //             s); 2]
                        //         } else {
                        //             [' '.fg(None, s); 2]
                        //         };

                        //         for i in 0..width {
                        //             write!(w, "{}", if i == 0 { c } else
                        //             { tail })?;
                        //         }
                        //     }
                        //     write!(w, "\n")?;
                        // }

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
                                int width = chars == line.end()
                                              ? 1
                                              : self.config
                                                    .char_width(
                                                        *chars, col)
                                                    .second;

                                auto vbar      = get_vbar(col, row);
                                auto underline = get_underline(col)
                                                     .value_or(false);
                                auto [c, tail] = vbar.has_value() ? ([&] {
                                    auto vbar_ll = vbar.value();
                                    if (underline) {
                                        // TODO: Is this good?
                                        if (vbar_ll.label.span.length()
                                                <= 1
                                            || true) {
                                            return std::make_pair(
                                                draw.underbar,
                                                draw.underline);
                                        } else if (
                                            line.offset() + col
                                            == vbar_ll.label.span
                                                   .start()) {
                                            return std::make_pair(
                                                draw.ltop, draw.underbar);
                                        } else if (
                                            line.offset() + col
                                            == vbar_ll.label
                                                   .last_offset()) {
                                            return std::make_pair(
                                                draw.rtop, draw.underbar);
                                        } else {
                                            return std::make_pair(
                                                draw.underbar,
                                                draw.underline);
                                        }
                                    } else if (
                                        vbar_ll.multi && row == 0
                                        && self.config.multiline_arrows) {
                                        return std::make_pair(
                                            draw.uarrow, ' ');
                                    } else {
                                        return std::make_pair(
                                            draw.vbar, ' ');
                                    }
                                }())
                                                                  : (underline
                                                                         ? std::make_pair(
                                                                             draw.underline,
                                                                             draw.underline)
                                                                         : std::make_pair(
                                                                             ' ',
                                                                             ' '));
                                c    = c.fg(vbar.value().label.color, s);
                                tail = tail.fg(
                                    vbar.value().label.color, s);

                                for (int i = 0; i < width; ++i) {
                                    w << (i == 0 ? c : tail);
                                }
                            }
                            w << "\n";
                        }


                        // Margin
                        // write_margin(
                        //     &mut w,
                        //     idx,
                        //     false,
                        //     is_ellipsis,
                        //     true,
                        //     Some((row, true)),
                        //     &line_labels,
                        //     &margin_label,
                        // )?;
                        write_margin(
                            w,
                            idx,
                            false,
                            is_ellipsis,
                            true,
                            std::make_pair(row, true),
                            line_labels,
                            margin_label);


                        // Lines
                        // let mut chars = line.chars();
                        // for col in 0..arrow_len {
                        //     let width = chars.next().map_or(1, |c|
                        //     self.config.char_width(c, col).1);

                        //     let is_hbar = (((col > line_label.col) ^
                        //     line_label.multi)
                        //         || (line_label.label.msg.is_some()
                        //             && line_label.draw_msg
                        //             && col > line_label.col))
                        //         && line_label.label.msg.is_some();
                        //     let [c, tail] = if col == line_label.col
                        //         && line_label.label.msg.is_some()
                        //         && margin_label.as_ref().map_or(true,
                        //         |m| {
                        //             line_label.label as *const _ !=
                        //             m.label as *const _
                        //         }) {
                        //         [
                        //             if line_label.multi {
                        //                 if line_label.draw_msg {
                        //                     draw.mbot
                        //                 } else {
                        //                     draw.rbot
                        //                 }
                        //             } else {
                        //                 draw.lbot
                        //             }
                        //             .fg(line_label.label.color, s),
                        //             draw.hbar.fg(line_label.label.color,
                        //             s),
                        //         ]
                        //     } else if let Some(vbar_ll) = get_vbar(col,
                        //     row)
                        //         .filter(|_| (col != line_label.col ||
                        //         line_label.label.msg.is_some()))
                        //     {
                        //         if !self.config.cross_gap && is_hbar {
                        //             [
                        //                 draw.xbar.fg(line_label.label.color,
                        //                 s), '
                        //                 '.fg(line_label.label.color, s),
                        //             ]
                        //         } else if is_hbar {
                        //             [draw.hbar.fg(line_label.label.color,
                        //             s); 2]
                        //         } else {
                        //             [
                        //                 if vbar_ll.multi && row == 0 &&
                        //                 self.config.compact {
                        //                     draw.uarrow
                        //                 } else {
                        //                     draw.vbar
                        //                 }
                        //                 .fg(vbar_ll.label.color, s),
                        //                 ' '.fg(line_label.label.color,
                        //                 s),
                        //             ]
                        //         }
                        //     } else if is_hbar {
                        //         [draw.hbar.fg(line_label.label.color,
                        //         s); 2]
                        //     } else {
                        //         [' '.fg(None, s); 2]
                        //     };

                        //     if width > 0 {
                        //         write!(w, "{}", c)?;
                        //     }
                        //     for _ in 1..width {
                        //         write!(w, "{}", tail)?;
                        //     }
                        // }

                        auto chars = line.begin();
                        for (size_t col = 0; col < arrow_len; ++col) {
                            int width = chars == line.end()
                                          ? 1
                                          : self.config
                                                .char_width(*chars, col)
                                                .second;

                            bool is_hbar = (((col > line_label.col)
                                             ^ line_label.multi)
                                            || (line_label.label.msg
                                                    .has_value()
                                                && line_label.draw_msg
                                                && col > line_label.col))
                                        && line_label.label.msg
                                               .has_value();
                            auto [c, tail]
                                = (col == line_label.col
                                   && line_label.label.msg.has_value()
                                   && (!margin_label.has_value()
                                       || line_label.label
                                              != margin_label.value()
                                                     .label))
                                    ? (std::make_pair(
                                        (line_label.multi
                                             ? (line_label.draw_msg
                                                    ? draw.mbot
                                                    : draw.rbot)
                                             : draw.lbot)
                                            .fg(line_label.label.color, s),
                                        draw.hbar.fg(
                                            line_label.label.color, s)))
                                    : ([&]() {
                                          auto vbar_ll = get_vbar(
                                              col, row);
                                          if (vbar_ll.has_value()
                                              && (col != line_label.col
                                                  || line_label.label.msg
                                                         .has_value())) {
                                              if (!self.config.cross_gap
                                                  && is_hbar) {
                                                  return std::make_pair(
                                                      draw.xbar.fg(
                                                          line_label.label
                                                              .color,
                                                          s),
                                                      ' '.fg(
                                                          line_label.label
                                                              .color,
                                                          s));
                                              } else if (is_hbar) {
                                                  return std::make_pair(
                                                      draw.hbar.fg(
                                                          line_label.label
                                                              .color,
                                                          s),
                                                      draw.hbar.fg(
                                                          line_label.label
                                                              .color,
                                                          s));
                                              } else {
                                                  return std::make_pair(
                                                      (vbar_ll.value()
                                                                   .multi
                                                               && row == 0
                                                               && self.config
                                                                      .compact
                                                           ? draw.uarrow
                                                           : draw.vbar)
                                                          .fg(vbar_ll
                                                                  .value()
                                                                  .label
                                                                  .color,
                                                              s),
                                                      ' '.fg(
                                                          line_label.label
                                                              .color,
                                                          s));
                                              }
                                          } else if (is_hbar) {
                                              return std::make_pair(
                                                  draw.hbar.fg(
                                                      line_label.label
                                                          .color,
                                                      s),
                                                  draw.hbar.fg(
                                                      line_label.label
                                                          .color,
                                                      s));
                                          } else {
                                              return std::make_pair(
                                                  ' '.fg(std::nullopt, s),
                                                  ' '.fg(std::nullopt, s));
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
                            .draw_msg {
                                write !(
                                    w,
                                    " {}",
                                    Show(line_label.label.msg.as_ref()))
                                    ? ;
                            }
                        }
                        write !(w, "\n") ? ;
                    }
                }
                }
            }

            // let is_final_group = group_idx + 1 == groups_len;
            bool is_final_group = group_idx + 1 == groups_len;

            // Help
            // if let (Some(note), true) = (&self.help, is_final_group) {
            //     if !self.config.compact {
            //         write_margin(&mut w, 0, false, false, true, Some((0,
            //         false)), &[], &None)?; write!(w, "\n")?;
            //     }
            //     write_margin(&mut w, 0, false, false, true, Some((0,
            //     false)), &[], &None)?; write!(w, "{}: {}\n",
            //     "Help".fg(self.config.note_color(), s), note)?;
            // }

            if (self.help.has_value() && is_final_group) {
                if (!self.config.compact) {
                write_margin(
                    w,
                    0,
                    false,
                    false,
                    true,
                    std::make_pair(0, false),
                    std::vector<Label>(),
                    std::nullopt);
                w << "\n";
                }
                write_margin(
                    w,
                    0,
                    false,
                    false,
                    true,
                    std::make_pair(0, false),
                    std::vector<Label>(),
                    std::nullopt);
                w << "Help".fg(self.config.note_color(), s) << ": "
                  << self.help.value() << "\n";
            }


            // Note
            // if let (Some(note), true) = (&self.note, is_final_group) {
            //     if !self.config.compact {
            //         write_margin(&mut w, 0, false, false, true, Some((0,
            //         false)), &[], &None)?; write!(w, "\n")?;
            //     }
            //     write_margin(&mut w, 0, false, false, true, Some((0,
            //     false)), &[], &None)?; write!(w, "{}: {}\n",
            //     "Note".fg(self.config.note_color(), s), note)?;
            // }

            if (self.note.has_value() && is_final_group) {
                if (!self.config.compact) {
                write_margin(
                    w,
                    0,
                    false,
                    false,
                    true,
                    std::make_pair(0, false),
                    std::vector<Label>(),
                    std::nullopt);
                w << "\n";
                }
                write_margin(
                    w,
                    0,
                    false,
                    false,
                    true,
                    std::make_pair(0, false),
                    std::vector<Label>(),
                    std::nullopt);
                w << "Note".fg(self.config.note_color(), s) << ": "
                  << self.note.value() << "\n";
            }

            // Tail of report
            // if !self.config.compact {
            //     if is_final_group {
            //         let final_margin =
            //             format!("{}{}", Show((draw.hbar, line_no_width +
            //             2)), draw.rbot);
            //         writeln!(w, "{}",
            //         final_margin.fg(self.config.margin_color(), s))?;
            //     } else {
            //         writeln!(
            //             w,
            //             "{}{}",
            //             Show((' ', line_no_width + 2)),
            //             draw.vbar.fg(self.config.margin_color(), s)
            //         )?;
            //     }
            // }

            if (!self.config.compact) {
                if (is_final_group) {
                std::string final_margin = format(
                    "{}{}", Show(draw.hbar, line_no_width + 2), draw.rbot);
                w << final_margin.fg(self.config.margin_color(), s)
                  << std::endl;
                } else {
                w << Show(' ', line_no_width + 2)
                  << draw.vbar.fg(self.config.margin_color(), s)
                  << std::endl;
                }
            }
            }
        }
    }
};
