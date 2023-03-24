#include <vector>
#include <utility>
#include <unordered_set>
#include <variant>
#include <functional>

template <typename T>
std::unordered_set<T> operator-(
    const std::unordered_set<T>& a,
    const std::unordered_set<T>& b) {
    std::unordered_set<T> result;
    for (const auto& elem : a) {
        if (b.find(elem) == b.end()) {
            result.insert(elem);
        }
    }
    return result;
}

template <typename T>
struct AlignElem {
    bool isGap = false;
    int  idx;
    T    item;
};

template <typename T>
using Align = std::vector<AlignElem<T>>;

template <typename T>
using AlignSeq = std::tuple<Align<T>, Align<T>, int>;

template <typename T>
using AlignGroup = std::vector<std::pair<int, Align<T>>>;

template <typename T>
using EqCmpProc = std::function<bool(const T&, const T&)>;

template <typename T>
using ScoreProc = std::function<int(const T&)>;

template <typename T>
using ScoreCmpProc = std::function<int(const T&, const T&)>;


template <typename T>
AlignSeq<T> needlemanWunschAlign(
    const std::vector<T>&                  seq1,
    const std::vector<T>&                  seq2,
    int                                    gapPenalty,
    std::function<int(const T&, const T&)> match_score) {
    AlignSeq<T> result;
    auto        score = newSeqWith(
        seq2.size() + 1, newSeqWith(seq1.size() + 1, 0));

    for (size_t i = 0; i <= seq2.size(); i++) {
        score[i][0] = gapPenalty * i;
    }

    for (size_t j = 0; j <= seq1.size(); j++) {
        score[0][j] = gapPenalty * j;
    }

    // Fill out all other values in the score matrix
    for (size_t i = 1; i <= seq2.size(); i++) {
        for (size_t j = 1; j <= seq1.size(); j++) {
            // Record the maximum score from the three possible scores
            // calculated above
            score[i][j] = std::max(
                {score[i - 1][j - 1]
                     + match_score(seq1[j - 1], seq2[i - 1]),
                 score[i - 1][j] + gapPenalty,
                 score[i][j - 1] + gapPenalty});
        }
    }

    size_t i = seq2.size();
    size_t j = seq1.size();
    while (i > 0 && j > 0) {
        auto curr  = score[i][j];
        auto diag  = score[i - 1][j - 1];
        auto vert  = score[i][j - 1];
        auto horiz = score[i - 1][j];

        if (curr == diag + match_score(seq1[j - 1], seq2[i - 1])) {
            result.align1.insert(
                result.align1.begin(), {j - 1, seq1[j - 1]});
            result.align2.insert(
                result.align2.begin(), {i - 1, seq2[i - 1]});

            i--;
            j--;
        } else if (curr == vert + gapPenalty) {
            result.align1.insert(
                result.align1.begin(), {j - 1, seq1[j - 1]});
            result.align2.insert(result.align2.begin(), {true});

            j--;
        } else if (curr == horiz + gapPenalty) {
            result.align1.insert(result.align1.begin(), {true});
            result.align2.insert(
                result.align2.begin(), {i - 1, seq2[i - 1]});

            i--;
        }
    }

    while (j > 0) {
        result.align1.insert(result.align1.begin(), {j - 1, seq1[j - 1]});
        result.align2.insert(result.align2.begin(), {true});

        j--;
    }

    while (i > 0) {
        result.align1.insert(result.align1.begin(), {true});
        result.align2.insert(result.align2.begin(), {i - 1, seq2[i - 1]});

        i--;
    }

    result.score = score.back().back();
    return result;
}

template <typename T>
AlignSeq<T> firstAlign(
    const std::vector<T>& seq1,
    const std::vector<T>& seq2,
    int                   gapOpenPenalty = -2,
    int                   gapExtPenalty  = -1,
    ScoreCmpProc<T>       matchScore     = nullptr) {
    for (const auto& align :
         allAlign(seq1, seq2, gapOpenPenalty, gapExtPenalty, matchScore)) {
        return align;
    }
    return {};
}

template <typename T>
Align<T> alignToGroup(
    AlignGroup<T>&         group,
    const Align<T>&        seqN,
    const ScoreCmpProc<T>& matchScore,
    const int              gapToGapPenalty  = -1,
    const int              gapOpenPenalty   = -2,
    const int              gapExtPenalty    = -1,
    const ScoreProc<T>&    gapToItemPenalty = ScoreProc<T>()) {
    int      bestScore = 0;
    Align<T> result;
    for (size_t idx = 0; idx < group.size(); ++idx) {
        const Align<T>& align = group[idx].second;
        AlignResult<T>  tmp   = firstAlign(
            align.align,
            seqN,
            [&](const AlignElem<T>& el1, const AlignElem<T>& el2) -> int {
                if (el1.isGap && el2.isGap) {
                    return gapToGapPenalty;
                } else if (!el1.isGap) {
                    return gapToItemPenalty(el1.item);
                } else if (!el2.isGap) {
                    return gapToItemPenalty(el2.item);
                }
                return matchScore(el1.item, el2.item);
            },
            gapOpenPenalty,
            gapExtPenalty);

        if (idx == 0) {
            result    = tmp.align2.flatten();
            bestScore = tmp.score;
        } else {
            if (tmp.score > bestScore) {
                result    = tmp.align2.flatten();
                bestScore = tmp.score;
            }
        }
    }
    return result;
}


std::vector<Align<char>> bartonSternbergAlign(
    std::vector<std::vector<char>> seqs,
    ScoreCmpProc<char>             matchScore,
    ScoreProc<char>                gapToItemPenalty,
    int                            gapOpenPenalty    = -2,
    int                            gapToGapPenalty   = -2,
    int                            realignIterations = 2) {

    int maxLen = std::max_element(
                     seqs.begin(),
                     seqs.end(),
                     [](auto& a, auto& b) { return a.size() < b.size(); })
                     ->size();

    std::unordered_set<int> allIndices;
    for (int i = 0; i < seqs.size(); ++i) {
        allIndices.insert(i);
    }

    AlignGroup<char>          group;
    std::tuple<int, int, int> maxPos;
    Align<char>               al1, al2;
    bool                      isFirst = true;
    for (int i = 0; i < seqs.size(); ++i) {
        for (int j = 0; j < seqs.size(); ++j) {
            std::tie(al1, al2, std::get<2>(maxPos)) = needlemanWunschAlign(
                seqs[i], seqs[j], gapOpenPenalty, matchScore);
            if ((std::get<2>(maxPos) > std::get<2>(maxPos) or isFirst)
                and i != j) {
                isFirst             = false;
                std::get<0>(maxPos) = i;
                std::get<1>(maxPos) = j;
            }
        }
    }

    auto addedSeqs = std::unordered_set<int>{
        std::get<0>(maxPos), std::get<1>(maxPos)};
    group.push_back({std::get<0>(maxPos), al1});
    group.push_back({std::get<1>(maxPos), al2});

    while ((allIndices - addedSeqs).size() > 0) {
        int index = *std::min_element(
            allIndices.begin(), allIndices.end());
        Align<char> align = alignToGroup(
            group,
            seqs[index],
            gapToItemPenalty,
            gapToGapPenalty,
            matchScore);
        group.add({index, align});
        addedSeqs.insert(index);
    }

    for (int _ = 0; _ < realignIterations; ++_) {
        for (int i = 0; i < group.high; ++i) {
            Align<char> align = alignToGroup(
                group.slice(0, i).concat(group.slice(i + 1, group.high)),
                group[i].align,
                gapToItemPenalty,
                matchScore);
            group[i] = std::make_pair(group[i].idx, align);
        }
    }

    std::vector<Align<char>> result;
    for (auto entry : group) {
        result.push_back(entry.align);
    }
    return result;
}
