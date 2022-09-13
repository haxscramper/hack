#include <iostream>
#include <cassert>
#include <queue>
#include <algorithm>
#include <unordered_set>
#include <limits>
#include <functional>
#include <memory>
#include <optional>
#include <variant>
#include <vector>

/// \brief Within a tree, this identifies a node by its preorder offset.
struct NodeId {
  private:
    static constexpr int InvalidNodeOffset = -1;

  public:
    int Offset; /// Offset in the postorder iteratio
    inline NodeId() : Offset(InvalidNodeOffset) {}
    inline NodeId(int Offset) : Offset(Offset) {}

    inline         operator int() const { return Offset; }
    inline NodeId& operator++() { return ++Offset, *this; }
    inline NodeId& operator--() { return --Offset, *this; }
    // Support defining iterators on NodeId.
    inline NodeId& operator*() { return *this; }
    inline bool    isValid() const { return Offset != InvalidNodeOffset; }
    inline bool isInvalid() const { return Offset == InvalidNodeOffset; }
};

template <typename IdT>
class Mapping {
  public:
    Mapping()                           = default;
    Mapping(Mapping&& Other)            = default;
    Mapping& operator=(Mapping&& Other) = default;
    Mapping(size_t Size) {
        SrcToDst = std::make_unique<NodeId[]>(Size);
        DstToSrc = std::make_unique<NodeId[]>(Size);
    }
    void link(NodeId Src, NodeId Dst) {
        SrcToDst[Src] = Dst, DstToSrc[Dst] = Src;
    }
    NodeId getDst(NodeId Src) const { return SrcToDst[Src]; }
    NodeId getSrc(NodeId Dst) const { return DstToSrc[Dst]; }
    bool   hasSrc(NodeId Src) const { return getDst(Src).isValid(); }
    bool   hasDst(NodeId Dst) const { return getSrc(Dst).isValid(); }

  private:
    std::unique_ptr<NodeId[]> SrcToDst, DstToSrc;
};

enum class ChangeKind {
    None,
    Delete, // (Src): delete node Src.
    Update, // (Src, Dst): update the value of node Src to match Dst.
    Insert, // (Src, Dst, Pos): insert Src as child of Dst at offset Pos.
    Move, // (Src, Dst, Pos): move Src to be a child of Dst at offset Pos.
    UpdateMove // Same as Move plus Update.
};

struct ASTNodeKind {
    int value;
    ASTNodeKind(int kind) : value(kind) {}
    bool operator==(ASTNodeKind const& other) const {
        return value == other.value;
    }
};


template <typename IdT, typename ValT>
struct Node;

template <typename IdT, typename ValT>
struct ComparisonOptions {
    /// During top-down matching, only consider nodes of at least this
    /// height.
    int MinHeight = 2;
    /// During bottom-up matching, match only nodes with at least this
    /// value as the ratio of their common descendants.
    double MinSimilarity = 0.5;
    /// Whenever two subtrees are matched in the bottom-up phase, the
    /// optimal mapping is computed, unless the size of either subtrees
    /// exceeds this.
    int  MaxSize          = 100;
    bool StopAfterTopDown = false;
    /// Returns false if the nodes should never be matched.
    bool isMatchingAllowed(
        const Node<IdT, ValT>& N1,
        const Node<IdT, ValT>& N2) const {
        // IMPLEMENT customization for this comparison - sometimes
        // different node kinds can also be matched together (for example
        // two integer literal nodes)
        return N1.getNodeKind(*this) == N2.getNodeKind(*this);
    }

    std::function<ValT(IdT)> getNodeValueImpl;
    std::function<int(IdT)>  getNodeKindImpl;

    ValT getNodeValue(IdT id) const { return getNodeValueImpl(id); }
    int  getNodeKind(IdT id) const { return getNodeKindImpl(id); }
};


template <typename IdT, typename ValT>
struct DynTypedNodeImpl {
    IdT                                                       id;
    std::vector<std::shared_ptr<DynTypedNodeImpl<IdT, ValT>>> subnodes;

    ValT getNodeValue(ComparisonOptions<IdT, ValT> const& opts) {
        return opts.getNodeValue(id);
    }

    ASTNodeKind getNodeKind(ComparisonOptions<IdT, ValT> const& opts) {
        return opts.getNodeKind(id);
    }
};


template <typename IdT, typename ValT>
using DynTypedNode = std::shared_ptr<DynTypedNodeImpl<IdT, ValT>>;

template <typename IdT, typename ValT>
inline DynTypedNode<IdT, ValT> ast(
    IdT                                         id,
    std::vector<DynTypedNode<IdT, ValT>> const& subnodes = {}) {

    return std::make_shared<DynTypedNodeImpl<IdT, ValT>>(
        DynTypedNodeImpl<IdT, ValT>{id, subnodes});
}

/// \brief Represents an AST node, alongside some additional information.
template <typename IdT, typename ValT>
struct Node {
    NodeId Parent, LeftMostDescendant, RightMostDescendant;
    int    Depth, Height, Shift = 0;
    /// Reference to the original AST node
    DynTypedNode<IdT, ValT> ASTNode;
    std::vector<NodeId>     Subnodes;
    ChangeKind              Change = ChangeKind::None;

    ASTNodeKind getNodeKind(
        ComparisonOptions<IdT, ValT> const& _opts) const {
        return ASTNode->getNodeKind(_opts);
    }

    bool isLeaf() const { return Subnodes.empty(); }
};

/// SyntaxTree objects represent subtrees of the AST.
///
/// There are only two instances of the SyntaxTree class during comparison
/// - destination and source. Structure is not recursive in tiself -
/// subnodes are determined based on the Node::Subnodes field which
/// explicitly stores list of subnode ids.
template <typename IdT, typename ValT>
class SyntaxTree {
  public:
    /// Constructs a tree from any AST node.
    SyntaxTree(
        ComparisonOptions<IdT, ValT> const& _opts,
        DynTypedNode<IdT, ValT>             Node)
        : TreeImpl(std::make_unique<Impl>(_opts, this, Node)) {}
    SyntaxTree(SyntaxTree<IdT, ValT>&& Other) = default;
    ~SyntaxTree()                             = default;
    int    getSize() const { return TreeImpl->getSize(); }
    NodeId getRootId() const { return TreeImpl->getRootId(); }
    using PreorderIterator = NodeId;
    PreorderIterator       begin() const { return TreeImpl->begin(); }
    PreorderIterator       end() const { return TreeImpl->end(); }
    const Node<IdT, ValT>& getNode(NodeId Id) const {
        return TreeImpl->getNode(Id);
    }

    ComparisonOptions<IdT, ValT> const& getOpts() const {
        return TreeImpl->opts;
    }

    int findPositionInParent(NodeId Id) const {
        return TreeImpl->findPositionInParent(Id);
    }
    /// Serialize the node attributes to a string representation. This
    /// should uniquely distinguish nodes of the same kind. Note that this
    /// function just returns a representation of the node value, not
    /// considering descendants.
    ValT getNodeValue(NodeId Id) const {
        return TreeImpl->getNodeValue(Id);
    }
    ValT getNodeValue(const Node<IdT, ValT>& Node) const {
        return TreeImpl->getNodeValue(Node);
    }
    class Impl;
    std::unique_ptr<Impl> TreeImpl;
};


template <typename IdT, typename ValT>
class ASTDiff {
  public:
    ASTDiff(
        SyntaxTree<IdT, ValT>&              Src,
        SyntaxTree<IdT, ValT>&              Dst,
        const ComparisonOptions<IdT, ValT>& Options)
        : DiffImpl(std::make_unique<Impl>(
              *Src.TreeImpl,
              *Dst.TreeImpl,
              Options)) {}

    ~ASTDiff() = default;
    // Returns the ID of the node that is mapped to the given node in
    // SourceTree.
    NodeId getMapped(const SyntaxTree<IdT, ValT>& SourceTree, NodeId Id)
        const {
        return DiffImpl->getMapped(SourceTree.TreeImpl, Id);
    }
    class Impl;

  private:
    std::unique_ptr<Impl> DiffImpl;
};

template <typename IdT, typename ValT>
class ASTDiff<IdT, ValT>::Impl {
  public:
    typename SyntaxTree<IdT, ValT>::Impl &T1, &T2;
    Mapping<IdT>                          TheMapping;
    Impl(
        typename SyntaxTree<IdT, ValT>::Impl& T1,
        typename SyntaxTree<IdT, ValT>::Impl& T2,
        const ComparisonOptions<IdT, ValT>&   Options)
        : T1(T1), T2(T2), Options(Options) {
        computeMapping();
        computeChangeKinds(TheMapping);
    }
    /// Matches nodes one-by-one based on their similarity.
    void computeMapping() {
        TheMapping = matchTopDown();
        if (Options.StopAfterTopDown) { return; }
        matchBottomUp(TheMapping);
    }
    // Compute Change for each node based on similarity.
    void   computeChangeKinds(Mapping<IdT>& M);
    NodeId getMapped(
        const std::unique_ptr<typename SyntaxTree<IdT, ValT>::Impl>& Tree,
        NodeId Id) const {
        if (&*Tree == &T1) { return TheMapping.getDst(Id); }
        assert(&*Tree == &T2 && "Invalid tree.");
        return TheMapping.getSrc(Id);
    }

  private:
    /// Returns true if the two subtrees are isomorphic to each other.
    bool identical(NodeId Id1, NodeId Id2) const;
    /// Returns false if the nodes must not be mached.
    bool isMatchingPossible(NodeId Id1, NodeId Id2) const {
        return Options.isMatchingAllowed(T1.getNode(Id1), T2.getNode(Id2));
    }
    /// Returns true if the nodes' parents are matched.
    bool haveSameParents(const Mapping<IdT>& M, NodeId Id1, NodeId Id2)
        const {
        NodeId P1 = T1.getNode(Id1).Parent;
        NodeId P2 = T2.getNode(Id2).Parent;
        return (P1.isInvalid() && P2.isInvalid()) ||
               (P1.isValid() && P2.isValid() && M.getDst(P1) == P2);
    }
    /// Uses an optimal albeit slow algorithm to compute a mapping<IdT>
    /// between two subtrees, but only if both have fewer nodes than
    /// MaxSize.
    void addOptimalMapping(Mapping<IdT>& M, NodeId Id1, NodeId Id2) const;
    // Computes the ratio of common descendants between the two nodes.
    // Descendants are only considered to be equal when they are mapped in
    // M.
    double getJaccardSimilarity(
        const Mapping<IdT>& M,
        NodeId              Id1,
        NodeId              Id2) const;
    // Returns the node that has the highest degree of similarity.
    NodeId findCandidate(const Mapping<IdT>& M, NodeId Id1) const;
    // Returns a mapping<IdT> of identical subtrees.
    Mapping<IdT> matchTopDown() const;
    // Tries to match any yet unmapped nodes, in a bottom-up fashion.
    void matchBottomUp(Mapping<IdT>& M) const;
    const ComparisonOptions<IdT, ValT>& Options;
    template <typename IdT_, typename ValT_>
    friend class ZhangShashaMatcher;
};

/// Represents the AST of a TranslationUnit.
template <typename IdT, typename ValT>
class SyntaxTree<IdT, ValT>::Impl {
  public:
    Impl(
        ComparisonOptions<IdT, ValT> const& opts,
        SyntaxTree<IdT, ValT>*              Parent);
    /// Constructs a tree from an AST node.
    Impl(
        ComparisonOptions<IdT, ValT> const& opts,
        SyntaxTree<IdT, ValT>*              Parent,
        DynTypedNode<IdT, ValT>             N);
    SyntaxTree<IdT, ValT>* Parent;
    /// Nodes in preorder.
    std::vector<Node<IdT, ValT>> Nodes;
    std::vector<NodeId>          Leaves;
    // Maps preorder indices to postorder ones.
    std::vector<int>             PostorderIds;
    std::vector<NodeId>          NodesBfs;
    int                          getSize() const { return Nodes.size(); }
    NodeId                       getRootId() const { return 0; }
    PreorderIterator             begin() const { return getRootId(); }
    PreorderIterator             end() const { return getSize(); }
    ComparisonOptions<IdT, ValT> opts;

    const Node<IdT, ValT>& getNode(NodeId Id) const { return Nodes[Id]; }

    Node<IdT, ValT>& getMutableNode(NodeId Id) { return Nodes[Id]; }

    bool isValidNodeId(NodeId Id) const {
        return 0 <= Id && Id < getSize();
    }

    void addNode(Node<IdT, ValT>& N) { Nodes.push_back(N); }

    int getNumberOfDescendants(NodeId Id) const {
        return getNode(Id).RightMostDescendant - Id + 1;
    }

    bool isInSubtree(NodeId Id, NodeId SubtreeRoot) const {
        return Id >= SubtreeRoot &&
               Id <= getNode(SubtreeRoot).RightMostDescendant;
    }

    int findPositionInParent(NodeId Id, bool Shifted = false) const {
        NodeId Parent = getNode(Id).Parent;
        if (Parent.isInvalid()) { return 0; }
        const auto& Siblings = getNode(Parent).Subnodes;
        int         Position = 0;
        for (size_t I = 0, E = Siblings.size(); I < E; ++I) {
            if (Shifted) { Position += getNode(Siblings[I]).Shift; }
            if (Siblings[I] == Id) {
                Position += I;
                return Position;
            }
        }

        assert(false && "Node not found in parent's children.");
    }

    ValT getNodeValue(NodeId Id) const {
        return getNodeValue(getNode(Id));
    }

    ValT getNodeValue(const Node<IdT, ValT>& Node) const {
        return Node.ASTNode->getNodeValue(opts);
    }

  private:
    void initTree() {
        setLeftMostDescendants();
        int PostorderId = 0;
        PostorderIds.resize(getSize());
        std::function<void(NodeId)> PostorderTraverse = [&](NodeId Id) {
            for (NodeId Subnode : getNode(Id).Subnodes) {
                PostorderTraverse(Subnode);
            }
            PostorderIds[Id] = PostorderId;
            ++PostorderId;
        };
        PostorderTraverse(getRootId());
        NodesBfs = getSubtreeBfs<IdT, ValT>(*this, getRootId());
    }

    void setLeftMostDescendants() {
        for (NodeId Leaf : Leaves) {
            getMutableNode(Leaf).LeftMostDescendant = Leaf;
            NodeId Parent, Cur = Leaf;
            while ((Parent = getNode(Cur).Parent).isValid() &&
                   getNode(Parent).Subnodes[0] == Cur) {
                Cur                                    = Parent;
                getMutableNode(Cur).LeftMostDescendant = Leaf;
            }
        }
    }
};

/// \brief Check whether a specific node should be ignored
template <typename IdT, typename ValT>
static bool isNodeExcluded(DynTypedNode<IdT, ValT>* node) {
    // HACK don't ignore anything for the purposes of testing - clang
    // implementation skips macros, things outside of the main file and
    // implicit nodes. Later on this should be turned into user-provided
    // callback.
    return false;
}

// Sets Height, Parent and Subnodes for each node.
template <typename IdT, typename ValT>
struct PreorderVisitor {
    int                                   Id = 0, Depth = 0;
    NodeId                                Parent;
    typename SyntaxTree<IdT, ValT>::Impl& Tree;
    PreorderVisitor(typename SyntaxTree<IdT, ValT>::Impl& Tree)
        : Tree(Tree) {}
    std::tuple<NodeId, NodeId> PreTraverse(DynTypedNode<IdT, ValT> node) {
        NodeId MyId = Id;
        Tree.Nodes.emplace_back();
        Node<IdT, ValT>& N = Tree.getMutableNode(MyId);
        N.Parent           = Parent;
        N.Depth            = Depth;
        N.ASTNode          = node;

        if (Parent.isValid()) {
            Node<IdT, ValT>& P = Tree.getMutableNode(Parent);
            P.Subnodes.push_back(MyId);
        }

        Parent = MyId;
        ++Id;
        ++Depth;
        return std::make_tuple(MyId, Tree.getNode(MyId).Parent);
    }

    void PostTraverse(std::tuple<NodeId, NodeId> State) {
        NodeId MyId, PreviousParent;
        std::tie(MyId, PreviousParent) = State;
        assert(
            MyId.isValid() && "Expecting to only traverse valid nodes.");
        Parent = PreviousParent;
        --Depth;
        Node<IdT, ValT>& N    = Tree.getMutableNode(MyId);
        N.RightMostDescendant = Id - 1;
        assert(
            N.RightMostDescendant >= 0 &&
            N.RightMostDescendant < Tree.getSize() &&
            "Rightmost descendant must be a valid tree node.");
        if (N.isLeaf()) Tree.Leaves.push_back(MyId);
        N.Height = 1;
        for (NodeId Subnode : N.Subnodes) {
            N.Height = std::max(
                N.Height, 1 + Tree.getNode(Subnode).Height);
        }
    }

    void Traverse(DynTypedNode<IdT, ValT> node) {
        auto SavedState = PreTraverse(node);
        for (auto sub : node->subnodes) {
            Traverse(sub);
        }
        PostTraverse(SavedState);
    }
};

template <typename IdT, typename ValT>
SyntaxTree<IdT, ValT>::Impl::Impl(
    ComparisonOptions<IdT, ValT> const& _opts,
    SyntaxTree<IdT, ValT>*              Parent)
    : Parent(Parent), opts(_opts) {}

template <typename IdT, typename ValT>
SyntaxTree<IdT, ValT>::Impl::Impl(
    ComparisonOptions<IdT, ValT> const& _opts,
    SyntaxTree<IdT, ValT>*              Parent,
    DynTypedNode<IdT, ValT>             N)
    : Impl(_opts, Parent) {
    PreorderVisitor<IdT, ValT> PreorderWalker{*this};
    PreorderWalker.Traverse(N);
    initTree();
}

template <typename IdT, typename ValT>
static std::vector<NodeId> getSubtreePostorder(
    const typename SyntaxTree<IdT, ValT>::Impl& Tree,
    NodeId                                      Root) {
    std::vector<NodeId>         Postorder;
    std::function<void(NodeId)> Traverse = [&](NodeId Id) {
        const Node<IdT, ValT>& N = Tree.getNode(Id);
        for (NodeId Subnode : N.Subnodes)
            Traverse(Subnode);
        Postorder.push_back(Id);
    };
    Traverse(Root);
    return Postorder;
}

template <typename IdT, typename ValT>
static std::vector<NodeId> getSubtreeBfs(
    const typename SyntaxTree<IdT, ValT>::Impl& Tree,
    NodeId                                      Root) {
    std::vector<NodeId> Ids;
    size_t              Expanded = 0;
    Ids.push_back(Root);
    while (Expanded < Ids.size()) {
        for (NodeId Subnode : Tree.getNode(Ids[Expanded++]).Subnodes) {
            Ids.push_back(Subnode);
        }
    }
    return Ids;
}


/// \brief Identifies a node in a subtree by its postorder offset, starting
/// at 1.
struct SNodeId {
    int Id = 0;
    explicit SNodeId(int Id) : Id(Id) {}
    explicit SNodeId() = default;
             operator int() const { return Id; }
    SNodeId& operator++() { return ++Id, *this; }
    SNodeId& operator--() { return --Id, *this; }
    SNodeId  operator+(int Other) const { return SNodeId(Id + Other); }
};


template <typename IdT, typename ValT>
class Subtree {
  private:
    /// The parent tree.
    const typename SyntaxTree<IdT, ValT>::Impl& Tree;
    /// Maps SNodeIds to original ids.
    std::vector<NodeId> RootIds;
    /// Maps subtree nodes to their leftmost descendants wtihin the
    /// subtree.
    std::vector<SNodeId> LeftMostDescendants;

  public:
    std::vector<SNodeId> KeyRoots;
    Subtree(
        const typename SyntaxTree<IdT, ValT>::Impl& Tree,
        NodeId                                      SubtreeRoot)
        : Tree(Tree) {
        RootIds       = getSubtreePostorder<IdT, ValT>(Tree, SubtreeRoot);
        int NumLeaves = setLeftMostDescendants();
        computeKeyRoots(NumLeaves);
    }

    int getSize() const { return RootIds.size(); }

    NodeId getIdInRoot(SNodeId Id) const {
        assert(Id > 0 && Id <= getSize() && "Invalid subtree node index.");
        return RootIds[Id - 1];
    }

    const Node<IdT, ValT>& getNode(SNodeId Id) const {
        return Tree.getNode(getIdInRoot(Id));
    }

    SNodeId getLeftMostDescendant(SNodeId Id) const {
        assert(Id > 0 && Id <= getSize() && "Invalid subtree node index.");
        return LeftMostDescendants[Id - 1];
    }
    /// Returns the postorder index of the leftmost descendant in the
    /// subtree.
    NodeId getPostorderOffset() const {
        return Tree.PostorderIds[getIdInRoot(SNodeId(1))];
    }

    ValT getNodeValue(SNodeId Id) const {
        return Tree.getNodeValue(getIdInRoot(Id));
    }

  private:
    /// Returns the number of leafs in the subtree.
    int setLeftMostDescendants() {
        int NumLeaves = 0;
        LeftMostDescendants.resize(getSize());
        for (int I = 0; I < getSize(); ++I) {
            SNodeId                SI(I + 1);
            const Node<IdT, ValT>& N = getNode(SI);
            NumLeaves += N.isLeaf();
            assert(
                I == Tree.PostorderIds[getIdInRoot(SI)] -
                         getPostorderOffset() &&
                "Postorder traversal in subtree should correspond to "
                "traversal in the root tree by a constant offset.");
            LeftMostDescendants[I] = SNodeId(
                Tree.PostorderIds[N.LeftMostDescendant] -
                getPostorderOffset());
        }
        return NumLeaves;
    }

    void computeKeyRoots(int Leaves) {
        KeyRoots.resize(Leaves);
        std::unordered_set<int> Visited;
        int                     K = Leaves - 1;
        for (SNodeId I(getSize()); I > 0; --I) {
            SNodeId LeftDesc = getLeftMostDescendant(I);
            if (Visited.count(LeftDesc)) continue;
            assert(K >= 0 && "K should be non-negative");
            KeyRoots[K] = I;
            Visited.insert(LeftDesc);
            --K;
        }
    }
};
/// Implementation of Zhang and Shasha's Algorithm for tree edit distance.
/// Computes an optimal mapping<IdT> between two trees using only
/// insertion, deletion and update as edit actions (similar to the
/// Levenshtein distance).
template <typename IdT, typename ValT>
class ZhangShashaMatcher {
    const typename ASTDiff<IdT, ValT>::Impl&     DiffImpl;
    Subtree<IdT, ValT>                           S1;
    Subtree<IdT, ValT>                           S2;
    std::unique_ptr<std::unique_ptr<double[]>[]> TreeDist, ForestDist;

  public:
    ZhangShashaMatcher(
        const typename ASTDiff<IdT, ValT>::Impl&    DiffImpl,
        const typename SyntaxTree<IdT, ValT>::Impl& T1,
        const typename SyntaxTree<IdT, ValT>::Impl& T2,
        NodeId                                      Id1,
        NodeId                                      Id2)
        : DiffImpl(DiffImpl), S1(T1, Id1), S2(T2, Id2) {
        TreeDist = std::make_unique<std::unique_ptr<double[]>[]>(
            size_t(S1.getSize()) + 1);
        ForestDist = std::make_unique<std::unique_ptr<double[]>[]>(
            size_t(S1.getSize()) + 1);
        for (int I = 0, E = S1.getSize() + 1; I < E; ++I) {
            TreeDist[I] = std::make_unique<double[]>(
                size_t(S2.getSize()) + 1);
            ForestDist[I] = std::make_unique<double[]>(
                size_t(S2.getSize()) + 1);
        }
    }

    std::vector<std::pair<NodeId, NodeId>> getMatchingNodes() {
        std::vector<std::pair<NodeId, NodeId>>   Matches;
        std::vector<std::pair<SNodeId, SNodeId>> TreePairs;
        computeTreeDist();
        bool RootNodePair = true;
        TreePairs.emplace_back(
            SNodeId(S1.getSize()), SNodeId(S2.getSize()));

        while (!TreePairs.empty()) {
            SNodeId LastRow, LastCol, FirstRow, FirstCol, Row, Col;
            std::tie(LastRow, LastCol) = TreePairs.back();
            TreePairs.pop_back();
            if (!RootNodePair) { computeForestDist(LastRow, LastCol); }
            RootNodePair = false;
            FirstRow     = S1.getLeftMostDescendant(LastRow);
            FirstCol     = S2.getLeftMostDescendant(LastCol);
            Row          = LastRow;
            Col          = LastCol;
            while (Row > FirstRow || Col > FirstCol) {
                if (Row > FirstRow &&
                    ForestDist[Row - 1][Col] + 1 == ForestDist[Row][Col]) {
                    --Row;
                } else if (
                    Col > FirstCol &&
                    ForestDist[Row][Col - 1] + 1 == ForestDist[Row][Col]) {
                    --Col;
                } else {
                    SNodeId LMD1 = S1.getLeftMostDescendant(Row);
                    SNodeId LMD2 = S2.getLeftMostDescendant(Col);
                    if (LMD1 == S1.getLeftMostDescendant(LastRow) &&
                        LMD2 == S2.getLeftMostDescendant(LastCol)) {
                        NodeId Id1 = S1.getIdInRoot(Row);
                        NodeId Id2 = S2.getIdInRoot(Col);
                        assert(
                            DiffImpl.isMatchingPossible(Id1, Id2) &&
                            "These nodes must not be matched.");
                        Matches.emplace_back(Id1, Id2);
                        --Row;
                        --Col;
                    } else {
                        TreePairs.emplace_back(Row, Col);
                        Row = LMD1;
                        Col = LMD2;
                    }
                }
            }
        }
        return Matches;
    }

  private:
    /// We use a simple cost model for edit actions, which seems good
    /// enough. Simple cost model for edit actions. This seems to make the
    /// matching algorithm perform reasonably well. The values range
    /// between 0 and 1, or infinity if this edit action should always be
    /// avoided.
    static constexpr double DeletionCost  = 1;
    static constexpr double InsertionCost = 1;
    double                  getUpdateCost(SNodeId Id1, SNodeId Id2) {
                         if (!DiffImpl.isMatchingPossible(
                S1.getIdInRoot(Id1), S2.getIdInRoot(Id2))) {
                             return std::numeric_limits<double>::max();
        } else {
                             return S1.getNodeValue(Id1) != S2.getNodeValue(Id2);
        }
    }

    void computeTreeDist() {
        for (SNodeId Id1 : S1.KeyRoots) {
            for (SNodeId Id2 : S2.KeyRoots) {
                computeForestDist(Id1, Id2);
            }
        }
    }

    void computeForestDist(SNodeId Id1, SNodeId Id2) {
        assert(Id1 > 0 && Id2 > 0 && "Expecting offsets greater than 0.");
        SNodeId LMD1           = S1.getLeftMostDescendant(Id1);
        SNodeId LMD2           = S2.getLeftMostDescendant(Id2);
        ForestDist[LMD1][LMD2] = 0;
        for (SNodeId D1 = LMD1 + 1; D1 <= Id1; ++D1) {
            ForestDist[D1][LMD2] = ForestDist[D1 - 1][LMD2] + DeletionCost;
            for (SNodeId D2 = LMD2 + 1; D2 <= Id2; ++D2) {
                ForestDist[LMD1][D2] = ForestDist[LMD1][D2 - 1] +
                                       InsertionCost;
                SNodeId DLMD1 = S1.getLeftMostDescendant(D1);
                SNodeId DLMD2 = S2.getLeftMostDescendant(D2);
                if (DLMD1 == LMD1 && DLMD2 == LMD2) {
                    double UpdateCost  = getUpdateCost(D1, D2);
                    ForestDist[D1][D2] = std::min(
                        {ForestDist[D1 - 1][D2] + DeletionCost,
                         ForestDist[D1][D2 - 1] + InsertionCost,
                         ForestDist[D1 - 1][D2 - 1] + UpdateCost});
                    TreeDist[D1][D2] = ForestDist[D1][D2];
                } else {
                    ForestDist[D1][D2] = std::min(
                        {ForestDist[D1 - 1][D2] + DeletionCost,
                         ForestDist[D1][D2 - 1] + InsertionCost,
                         ForestDist[DLMD1][DLMD2] + TreeDist[D1][D2]});
                }
            }
        }
    }
};

// Compares nodes by their depth.
template <typename IdT, typename ValT>
struct HeightLess {
    const typename SyntaxTree<IdT, ValT>::Impl& Tree;
    HeightLess(const typename SyntaxTree<IdT, ValT>::Impl& Tree)
        : Tree(Tree) {}
    bool operator()(NodeId Id1, NodeId Id2) const {
        return Tree.getNode(Id1).Height < Tree.getNode(Id2).Height;
    }
};

// Priority queue for nodes, sorted descendingly by their height.
template <typename IdT, typename ValT>
class PriorityList {
    const typename SyntaxTree<IdT, ValT>::Impl& Tree;
    HeightLess<IdT, ValT>                       Cmp;
    std::vector<NodeId>                         Container;
    std::priority_queue<NodeId, std::vector<NodeId>, HeightLess<IdT, ValT>>
        List;

  public:
    PriorityList(const typename SyntaxTree<IdT, ValT>::Impl& Tree)
        : Tree(Tree), Cmp(Tree), List(Cmp, Container) {}
    void                push(NodeId id) { List.push(id); }
    std::vector<NodeId> pop() {
        int                 Max = peekMax();
        std::vector<NodeId> Result;
        if (Max == 0) { return Result; }
        while (peekMax() == Max) {
            Result.push_back(List.top());
            List.pop();
        }
        // TODO this is here to get a stable output, not a good heuristic
        std::sort(Result.begin(), Result.end());
        return Result;
    }


    int peekMax() const {
        if (List.empty()) { return 0; }
        return Tree.getNode(List.top()).Height;
    }

    /// \brief add all subnodes in the input list
    void open(NodeId Id) {
        for (NodeId Subnode : Tree.getNode(Id).Subnodes) {
            push(Subnode);
        }
    }
};


template <typename IdT, typename ValT>
bool ASTDiff<IdT, ValT>::Impl::identical(NodeId Id1, NodeId Id2) const {
    const Node<IdT, ValT>& N1 = T1.getNode(Id1);
    const Node<IdT, ValT>& N2 = T2.getNode(Id2);
    if (N1.Subnodes.size() != N2.Subnodes.size() ||
        !isMatchingPossible(Id1, Id2) ||
        T1.getNodeValue(Id1) != T2.getNodeValue(Id2)) {
        return false;
    }
    for (size_t Id = 0, E = N1.Subnodes.size(); Id < E; ++Id) {
        if (!identical(N1.Subnodes[Id], N2.Subnodes[Id])) { return false; }
    }
    return true;
}


template <typename IdT, typename ValT>
void ASTDiff<IdT, ValT>::Impl::addOptimalMapping(
    Mapping<IdT>& M,
    NodeId        Id1,
    NodeId        Id2) const {
    if (std::max(
            T1.getNumberOfDescendants(Id1),
            T2.getNumberOfDescendants(Id2)) > Options.MaxSize) {
        return;
    }
    ZhangShashaMatcher<IdT, ValT> Matcher(*this, T1, T2, Id1, Id2);
    std::vector<std::pair<NodeId, NodeId>> R = Matcher.getMatchingNodes();
    for (const auto& Tuple : R) {
        NodeId Src = Tuple.first;
        NodeId Dst = Tuple.second;
        if (!M.hasSrc(Src) && !M.hasDst(Dst)) { M.link(Src, Dst); }
    }
}

template <typename IdT, typename ValT>
double ASTDiff<IdT, ValT>::Impl::getJaccardSimilarity(
    const Mapping<IdT>& M,
    NodeId              Id1,
    NodeId              Id2) const {
    int                    CommonDescendants = 0;
    const Node<IdT, ValT>& N1                = T1.getNode(Id1);
    // Count the common descendants, excluding the subtree root.
    for (NodeId Src = Id1 + 1; Src <= N1.RightMostDescendant; ++Src) {
        NodeId Dst = M.getDst(Src);
        CommonDescendants += int(
            Dst.isValid() && T2.isInSubtree(Dst, Id2));
    }
    // We need to subtract 1 to get the number of descendants excluding the
    // root.
    double Denominator = T1.getNumberOfDescendants(Id1) - 1 +
                         T2.getNumberOfDescendants(Id2) - 1 -
                         CommonDescendants;
    // CommonDescendants is less than the size of one subtree.
    assert(Denominator >= 0 && "Expected non-negative denominator.");
    if (Denominator == 0) { return 0; }
    return CommonDescendants / Denominator;
}


template <typename IdT, typename ValT>
NodeId ASTDiff<IdT, ValT>::Impl::findCandidate(
    const Mapping<IdT>& M,
    NodeId              Id1) const {
    NodeId Candidate;
    double HighestSimilarity = 0.0;
    for (NodeId Id2 : T2) {
        if (!isMatchingPossible(Id1, Id2)) { continue; }
        if (M.hasDst(Id2)) { continue; }
        double Similarity = getJaccardSimilarity(M, Id1, Id2);
        if (Similarity >= Options.MinSimilarity &&
            Similarity > HighestSimilarity) {
            HighestSimilarity = Similarity;
            Candidate         = Id2;
        }
    }
    return Candidate;
}


template <typename IdT, typename ValT>
void ASTDiff<IdT, ValT>::Impl::matchBottomUp(Mapping<IdT>& M) const {
    std::vector<NodeId> Postorder = getSubtreePostorder<IdT, ValT>(
        T1, T1.getRootId());
    // for all nodes in left, if node itself is not matched, but
    // has any children matched
    for (NodeId Id1 : Postorder) {
        if (Id1 == T1.getRootId() && !M.hasSrc(T1.getRootId()) &&
            !M.hasDst(T2.getRootId())) {
            if (isMatchingPossible(T1.getRootId(), T2.getRootId())) {
                M.link(T1.getRootId(), T2.getRootId());
                addOptimalMapping(M, T1.getRootId(), T2.getRootId());
            }
            break;
        }
        bool                   Matched = M.hasSrc(Id1);
        const Node<IdT, ValT>& N1      = T1.getNode(Id1);

        bool MatchedSubnodes = std::any_of(
            N1.Subnodes.begin(), N1.Subnodes.end(), [&](NodeId Subnode) {
                return M.hasSrc(Subnode);
            });

        //  if it is a valid candidate and matches criteria for
        // minimum number of shares subnodes
        if (Matched || !MatchedSubnodes) { continue; }
        NodeId Id2 = findCandidate(M, Id1);
        if (Id2.isValid()) {
            // add node to mapping<IdT>
            M.link(Id1, Id2);
            // if max of number of subnodes does not exceed threshold
            addOptimalMapping(M, Id1, Id2);
        }
    }
}


template <typename IdT, typename ValT>
Mapping<IdT> ASTDiff<IdT, ValT>::Impl::matchTopDown() const {
    PriorityList<IdT, ValT> L1(T1);
    PriorityList<IdT, ValT> L2(T2);
    Mapping<IdT>            M(T1.getSize() + T2.getSize());
    L1.push(T1.getRootId());
    L2.push(T2.getRootId());
    int Max1, Max2;
    // until subtree of necessary height hasn't been reached
    while (std::min(Max1 = L1.peekMax(), Max2 = L2.peekMax()) >
           Options.MinHeight) {
        // if two top subtrees don't have equal height
        if (Max1 > Max2) {
            // insert all nodes from tallest subforest
            for (NodeId Id : L1.pop()) {
                L1.open(Id);
            }
        } else if (Max2 > Max1) {
            for (NodeId Id : L2.pop()) {
                L2.open(Id);
            }
        } else {
            // otherwise get two subforest of equal height
            std::vector<NodeId> H1, H2;
            H1 = L1.pop();
            H2 = L2.pop();
            // for each combination of Therese is these forests
            for (NodeId Id1 : H1) {
                for (NodeId Id2 : H2) {
                    // if pair of trees is isomorphic
                    if (identical(Id1, Id2) && !M.hasSrc(Id1) &&
                        !M.hasDst(Id2)) {
                        for (int I = 0, E = T1.getNumberOfDescendants(Id1);
                             I < E;
                             ++I) {
                            M.link(Id1 + I, Id2 + I);
                        }
                    }
                }
            }
            // so we basically determine if there is any isomorphic
            // mapping<IdT> between either (1) roots two highest subforests
            // or (2) root and subnodes of a root in other tree


            for (NodeId Id1 : H1) {
                // if there is unmatched forest root in first forest
                if (!M.hasSrc(Id1)) {
                    // insert it's subnodes
                    L1.open(Id1);
                }
            }
            for (NodeId Id2 : H2) {
                // do the same for other forest
                if (!M.hasDst(Id2)) { L2.open(Id2); }
            }
        }
    }
    return M;
}


template <typename IdT, typename ValT>
void ASTDiff<IdT, ValT>::Impl::computeChangeKinds(Mapping<IdT>& M) {
    for (NodeId Id1 : T1) {
        if (!M.hasSrc(Id1)) {
            T1.getMutableNode(Id1).Change = ChangeKind::Delete;
            T1.getMutableNode(Id1).Shift -= 1;
        }
    }
    for (NodeId Id2 : T2) {
        if (!M.hasDst(Id2)) {
            T2.getMutableNode(Id2).Change = ChangeKind::Insert;
            T2.getMutableNode(Id2).Shift -= 1;
        }
    }
    for (NodeId Id1 : T1.NodesBfs) {
        NodeId Id2 = M.getDst(Id1);
        if (Id2.isInvalid()) { continue; }
        if (!haveSameParents(M, Id1, Id2) ||
            T1.findPositionInParent(Id1, true) !=
                T2.findPositionInParent(Id2, true)) {
            T1.getMutableNode(Id1).Shift -= 1;
            T2.getMutableNode(Id2).Shift -= 1;
        }
    }
    for (NodeId Id2 : T2.NodesBfs) {
        NodeId Id1 = M.getSrc(Id2);
        if (Id1.isInvalid()) { continue; }
        Node<IdT, ValT>& N1 = T1.getMutableNode(Id1);
        Node<IdT, ValT>& N2 = T2.getMutableNode(Id2);
        if (Id1.isInvalid()) { continue; }
        if (!haveSameParents(M, Id1, Id2) ||
            T1.findPositionInParent(Id1, true) !=
                T2.findPositionInParent(Id2, true)) {
            N1.Change = N2.Change = ChangeKind::Move;
        }

        if (T1.getNodeValue(Id1) != T2.getNodeValue(Id2)) {
            N2.Change = (N1.Change == ChangeKind::Move ? ChangeKind::UpdateMove : ChangeKind::Update);
            N1.Change = N2.Change;
        }
    }
}

template <typename IdT, typename ValT>
static void printNode(
    std::ostream&          OS,
    SyntaxTree<IdT, ValT>& Tree,
    NodeId                 Id) {
    if (Id.isInvalid()) {
        OS << "None";
        return;
    }
    OS << Tree.getNode(Id).getNodeKind(Tree.getOpts()).value;
    std::string Value = Tree.getNodeValue(Id);
    if (!Value.empty()) { OS << ": " << Value; }
    OS << "(" << Id << ")";
}


template <typename IdT, typename ValT>
static void printDstChange(
    std::ostream&          OS,
    ASTDiff<IdT, ValT>&    Diff,
    SyntaxTree<IdT, ValT>& SrcTree,
    SyntaxTree<IdT, ValT>& DstTree,
    NodeId                 Dst) {
    const Node<IdT, ValT>& DstNode = DstTree.getNode(Dst);
    NodeId                 Src     = Diff.getMapped(DstTree, Dst);
    switch (DstNode.Change) {
        case ChangeKind::None: {
            break;
        }
        case ChangeKind::Delete: {
            assert(false && "The destination tree can't have deletions.");
        }
        case ChangeKind::Update: {
            OS << "Update ";
            OS << SrcTree.getNodeValue(Src);
            OS << " to " << DstTree.getNodeValue(Dst) << "\n";
            break;
        }
        case ChangeKind::Insert:
        case ChangeKind::Move:
        case ChangeKind::UpdateMove: {
            if (DstNode.Change == ChangeKind::Insert) {
                OS << "Insert";
            } else if (DstNode.Change == ChangeKind::Move) {
                OS << "Move";
            } else if (DstNode.Change == ChangeKind::UpdateMove) {
                OS << "Update and Move";
            }
            OS << " ";
            OS << DstTree.getNodeValue(Dst);
            OS << " into ";
            OS << DstTree.getNodeValue(DstNode.Parent);
            OS << " at " << DstTree.findPositionInParent(Dst) << "\n";
            break;
        }
    }
}

template <typename T>
std::ostream& operator<<(std::ostream& os, std::vector<T> const& vec) {
    os << "[";
    for (int i = 0; i < vec.size(); ++i) {
        if (0 < i) { os << ", "; }
        os << vec[i];
    }
    os << "]";
    return os;
}


int main() {
    {
        struct RealNode {
            std::string           value;
            int                   kind;
            std::vector<RealNode> sub;
        };

        using IdT  = RealNode*;
        using ValT = std::string;

        auto src = RealNode{
            "main",
            0,
            {RealNode{"sub-1", 1},
             RealNode{"sub-2", 2},
             RealNode{"subnode"}}};

        auto dst = RealNode{
            "main",
            0,
            {RealNode{"sub-1", 1},
             RealNode{"sub-2'", 2},
             RealNode{"sub-3", 3}}};

        auto Src = ast<IdT, ValT>(
            &src,
            {ast<IdT, ValT>(&src.sub[0]), ast<IdT, ValT>(&src.sub[1])});

        auto Dst = ast<IdT, ValT>(
            &dst,
            {ast<IdT, ValT>(&dst.sub[0]),
             ast<IdT, ValT>(&dst.sub[1]),
             ast<IdT, ValT>(&dst.sub[2])});

        ComparisonOptions<IdT, ValT> Options{
            .getNodeValueImpl = [](IdT id) { return id->value; },
            .getNodeKindImpl  = [](IdT id) { return id->kind; }};

        SyntaxTree<IdT, ValT> SrcTree{Options, Src};
        SyntaxTree<IdT, ValT> DstTree{Options, Dst};
        ASTDiff<IdT, ValT>    Diff{SrcTree, DstTree, Options};

        std::cout << SrcTree.TreeImpl->PostorderIds << "\n";
        std::cout << DstTree.TreeImpl->PostorderIds << "\n";

        for (NodeId Dst : DstTree) {
            NodeId Src = Diff.getMapped(DstTree, Dst);
            if (Src.isValid()) {
                std::cout << "Match ";
                printNode(std::cout, SrcTree, Src);
                std::cout << " to ";
                printNode(std::cout, DstTree, Dst);
                std::cout << "\n";
            }

            printDstChange(std::cout, Diff, SrcTree, DstTree, Dst);
        }
    }

    // {
    //     struct RealNode {
    //         std::variant<int, double, std::string> value;
    //         std::vector<RealNode>                  sub;
    //     };

    //     auto src = RealNode{
    //         "toplevel", {RealNode{1}, RealNode{1.2},
    //         RealNode{"subnode"}}};

    //     auto dst = RealNode{
    //         "toplevel",
    //         {RealNode{22}, RealNode{1.2}, RealNode{"subnode'"}}};

    //     using IdT  = RealNode*;
    //     using ValT = decltype(src.value);

    //     auto Src = ast<IdT, ValT>(
    //         "main",
    //         0,
    //         {ast<IdT, ValT>("sub-1", 1), ast<IdT, ValT>("sub-2", 1)});

    //     auto Dst = ast<IdT, ValT>(
    //         "main",
    //         0,
    //         {ast<IdT, ValT>("sub-1", 1),
    //          ast<IdT, ValT>("sub-2'", 1),
    //          ast<IdT, ValT>("sub-3", 1)});

    //     ComparisonOptions<IdT, ValT> Options{};
    //     SyntaxTree<IdT, ValT>        SrcTree{Src};
    //     SyntaxTree<IdT, ValT>        DstTree{Dst};
    //     ASTDiff<IdT, ValT>           Diff{SrcTree, DstTree, Options};

    //     for (NodeId Dst : DstTree) {
    //         NodeId Src = Diff.getMapped(DstTree, Dst);
    //         if (Src.isValid()) {
    //             std::cout << "Match ";
    //             printNode(std::cout, SrcTree, Src);
    //             std::cout << " to ";
    //             printNode(std::cout, DstTree, Dst);
    //             std::cout << "\n";
    //         }

    //         printDstChange(std::cout, Diff, SrcTree, DstTree, Dst);
    //     }
    // }


    std::cout << "diff done\n";

    return 0;
}
