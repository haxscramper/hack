#include <iostream>
#include <cassert>
#include <queue>
#include <algorithm>
#include <unordered_set>
#include <limits>
#include <functional>
#include <memory>
#include <optional>
#include <vector>

/// \brief Within a tree, this identifies a node by its preorder offset.
struct NodeId {
  private:
    static constexpr int InvalidNodeId = -1;

  public:
    int Id;
    inline NodeId() : Id(InvalidNodeId) {}
    inline NodeId(int Id) : Id(Id) {}
                   operator int() const { return Id; }
    inline NodeId& operator++() { return ++Id, *this; }
    inline NodeId& operator--() { return --Id, *this; }
    // Support defining iterators on NodeId.
    inline NodeId& operator*() { return *this; }
    inline bool    isValid() const { return Id != InvalidNodeId; }
    inline bool    isInvalid() const { return Id == InvalidNodeId; }
};

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

enum ChangeKind {
    None,
    Delete, // (Src): delete node Src.
    Update, // (Src, Dst): update the value of node Src to match Dst.
    Insert, // (Src, Dst, Pos): insert Src as child of Dst at offset Pos.
    Move, // (Src, Dst, Pos): move Src to be a child of Dst at offset Pos.
    UpdateMove // Same as Move plus Update.
};

enum class ASTNodeKind { ClassNone };
struct DynTypedNodeImpl {
    std::string                                    value;
    ASTNodeKind                                    kind;
    std::vector<std::shared_ptr<DynTypedNodeImpl>> subnodes;

    std::string getName() { return value; }
    ASTNodeKind getNodeKind() { return kind; }
};


using DynTypedNode = std::shared_ptr<DynTypedNodeImpl>;

/// \brief Represents a Clang AST node, alongside some additional
/// information.
struct Node {
    NodeId Parent, LeftMostDescendant, RightMostDescendant;
    int    Depth, Height, Shift = 0;
    /// Reference to the original AST node
    DynTypedNode               ASTNode;
    std::vector<NodeId>        Children;
    ChangeKind                 Change = None;
    ASTNodeKind                getType() const;
    std::string                getTypeLabel() const;
    bool                       isLeaf() const { return Children.empty(); }
    std::optional<std::string> getIdentifier() const;
    std::optional<std::string> getQualifiedIdentifier() const;
};

/// SyntaxTree objects represent subtrees of the AST.
/// They can be constructed from any Decl or Stmt.
class SyntaxTree {
  public:
    /// Constructs a tree from any AST node.
    template <class T>
    SyntaxTree(T* Node) : TreeImpl(std::make_unique<Impl>(this, Node)) {}
    SyntaxTree(SyntaxTree&& Other) = default;
    ~SyntaxTree();
    std::string getFilename() const;
    int         getSize() const;
    NodeId      getRootId() const;
    using PreorderIterator = NodeId;
    PreorderIterator begin() const;
    PreorderIterator end() const;
    const Node&      getNode(NodeId Id) const;
    int              findPositionInParent(NodeId Id) const;
    /// Serialize the node attributes to a string representation. This
    /// should uniquely distinguish nodes of the same kind. Note that this
    /// function just returns a representation of the node value, not
    /// considering descendants.
    std::string getNodeValue(NodeId Id) const;
    std::string getNodeValue(const Node& Node) const;
    class Impl;
    std::unique_ptr<Impl> TreeImpl;
};

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
    bool isMatchingAllowed(const Node& N1, const Node& N2) const {
        return N1.getType() == N2.getType();
    }
};


class ASTDiff {
  public:
    ASTDiff(
        SyntaxTree&              Src,
        SyntaxTree&              Dst,
        const ComparisonOptions& Options);
    ~ASTDiff();
    // Returns the ID of the node that is mapped to the given node in
    // SourceTree.
    NodeId getMapped(const SyntaxTree& SourceTree, NodeId Id) const;
    class Impl;

  private:
    std::unique_ptr<Impl> DiffImpl;
};

class ASTDiff::Impl {
  public:
    SyntaxTree::Impl &T1, &T2;
    Mapping           TheMapping;
    Impl(
        SyntaxTree::Impl&        T1,
        SyntaxTree::Impl&        T2,
        const ComparisonOptions& Options);
    /// Matches nodes one-by-one based on their similarity.
    void computeMapping();
    // Compute Change for each node based on similarity.
    void   computeChangeKinds(Mapping& M);
    NodeId getMapped(
        const std::unique_ptr<SyntaxTree::Impl>& Tree,
        NodeId                                   Id) const {
        if (&*Tree == &T1) return TheMapping.getDst(Id);
        assert(&*Tree == &T2 && "Invalid tree.");
        return TheMapping.getSrc(Id);
    }

  private:
    // Returns true if the two subtrees are identical.
    bool identical(NodeId Id1, NodeId Id2) const;
    // Returns false if the nodes must not be mached.
    bool isMatchingPossible(NodeId Id1, NodeId Id2) const;
    // Returns true if the nodes' parents are matched.
    bool haveSameParents(const Mapping& M, NodeId Id1, NodeId Id2) const;
    // Uses an optimal albeit slow algorithm to compute a mapping between
    // two subtrees, but only if both have fewer nodes than MaxSize.
    void addOptimalMapping(Mapping& M, NodeId Id1, NodeId Id2) const;
    // Computes the ratio of common descendants between the two nodes.
    // Descendants are only considered to be equal when they are mapped in
    // M.
    double getJaccardSimilarity(const Mapping& M, NodeId Id1, NodeId Id2)
        const;
    // Returns the node that has the highest degree of similarity.
    NodeId findCandidate(const Mapping& M, NodeId Id1) const;
    // Returns a mapping of identical subtrees.
    Mapping matchTopDown() const;
    // Tries to match any yet unmapped nodes, in a bottom-up fashion.
    void                     matchBottomUp(Mapping& M) const;
    const ComparisonOptions& Options;
    friend class ZhangShashaMatcher;
};

/// Represents the AST of a TranslationUnit.
class SyntaxTree::Impl {
  public:
    Impl(SyntaxTree* Parent);
    /// Constructs a tree from an AST node.
    Impl(SyntaxTree* Parent, DynTypedNode N);
    SyntaxTree* Parent;
    /// Nodes in preorder.
    std::vector<Node>   Nodes;
    std::vector<NodeId> Leaves;
    // Maps preorder indices to postorder ones.
    std::vector<int>    PostorderIds;
    std::vector<NodeId> NodesBfs;
    int                 getSize() const { return Nodes.size(); }
    NodeId              getRootId() const { return 0; }
    PreorderIterator    begin() const { return getRootId(); }
    PreorderIterator    end() const { return getSize(); }
    const Node&         getNode(NodeId Id) const { return Nodes[Id]; }
    Node&               getMutableNode(NodeId Id) { return Nodes[Id]; }
    bool                isValidNodeId(NodeId Id) const {
                       return Id >= 0 && Id < getSize();
    }
    void addNode(Node& N) { Nodes.push_back(N); }
    int  getNumberOfDescendants(NodeId Id) const;
    bool isInSubtree(NodeId Id, NodeId SubtreeRoot) const;
    int  findPositionInParent(NodeId Id, bool Shifted = false) const;
    std::string getRelativeName(DynTypedNode node) const;
    std::string getNodeValue(NodeId Id) const;
    std::string getNodeValue(const Node& Node) const;

  private:
    void initTree();
    void setLeftMostDescendants();
};

/// \brief Check whether a specific node should be ignored
static bool isNodeExcluded(DynTypedNode* node) {
    // HACK don't ignore anything for the purposes of testing - clang
    // implementation skips macros, things outside of the main file and
    // implicit nodes. Later on this should be turned into user-provided
    // callback.
    return false;
}

// Sets Height, Parent and Children for each node.
struct PreorderVisitor {
    int               Id = 0, Depth = 0;
    NodeId            Parent;
    SyntaxTree::Impl& Tree;
    PreorderVisitor(SyntaxTree::Impl& Tree) : Tree(Tree) {}
    std::tuple<NodeId, NodeId> PreTraverse(DynTypedNode node) {
        NodeId MyId = Id;
        Tree.Nodes.emplace_back();
        Node& N   = Tree.getMutableNode(MyId);
        N.Parent  = Parent;
        N.Depth   = Depth;
        N.ASTNode = node;

        if (Parent.isValid()) {
            Node& P = Tree.getMutableNode(Parent);
            P.Children.push_back(MyId);
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
        Node& N               = Tree.getMutableNode(MyId);
        N.RightMostDescendant = Id - 1;
        assert(
            N.RightMostDescendant >= 0 &&
            N.RightMostDescendant < Tree.getSize() &&
            "Rightmost descendant must be a valid tree node.");
        if (N.isLeaf()) Tree.Leaves.push_back(MyId);
        N.Height = 1;
        for (NodeId Child : N.Children)
            N.Height = std::max(N.Height, 1 + Tree.getNode(Child).Height);
    }

    void Traverse(DynTypedNode node) {
        auto SavedState = PreTraverse(node);
        for (auto sub : node->subnodes) {
            Traverse(sub);
        }
        PostTraverse(SavedState);
    }
};

SyntaxTree::Impl::Impl(SyntaxTree* Parent) : Parent(Parent) {}

SyntaxTree::Impl::Impl(SyntaxTree* Parent, DynTypedNode N) : Impl(Parent) {
    PreorderVisitor PreorderWalker(*this);
    PreorderWalker.Traverse(N);
    initTree();
}

static std::vector<NodeId> getSubtreePostorder(
    const SyntaxTree::Impl& Tree,
    NodeId                  Root) {
    std::vector<NodeId>         Postorder;
    std::function<void(NodeId)> Traverse = [&](NodeId Id) {
        const Node& N = Tree.getNode(Id);
        for (NodeId Child : N.Children)
            Traverse(Child);
        Postorder.push_back(Id);
    };
    Traverse(Root);
    return Postorder;
}
static std::vector<NodeId> getSubtreeBfs(
    const SyntaxTree::Impl& Tree,
    NodeId                  Root) {
    std::vector<NodeId> Ids;
    size_t              Expanded = 0;
    Ids.push_back(Root);
    while (Expanded < Ids.size())
        for (NodeId Child : Tree.getNode(Ids[Expanded++]).Children)
            Ids.push_back(Child);
    return Ids;
}
void SyntaxTree::Impl::initTree() {
    setLeftMostDescendants();
    int PostorderId = 0;
    PostorderIds.resize(getSize());
    std::function<void(NodeId)> PostorderTraverse = [&](NodeId Id) {
        for (NodeId Child : getNode(Id).Children)
            PostorderTraverse(Child);
        PostorderIds[Id] = PostorderId;
        ++PostorderId;
    };
    PostorderTraverse(getRootId());
    NodesBfs = getSubtreeBfs(*this, getRootId());
}
void SyntaxTree::Impl::setLeftMostDescendants() {
    for (NodeId Leaf : Leaves) {
        getMutableNode(Leaf).LeftMostDescendant = Leaf;
        NodeId Parent, Cur = Leaf;
        while ((Parent = getNode(Cur).Parent).isValid() &&
               getNode(Parent).Children[0] == Cur) {
            Cur                                    = Parent;
            getMutableNode(Cur).LeftMostDescendant = Leaf;
        }
    }
}
int SyntaxTree::Impl::getNumberOfDescendants(NodeId Id) const {
    return getNode(Id).RightMostDescendant - Id + 1;
}
bool SyntaxTree::Impl::isInSubtree(NodeId Id, NodeId SubtreeRoot) const {
    return Id >= SubtreeRoot &&
           Id <= getNode(SubtreeRoot).RightMostDescendant;
}
int SyntaxTree::Impl::findPositionInParent(NodeId Id, bool Shifted) const {
    NodeId Parent = getNode(Id).Parent;
    if (Parent.isInvalid()) return 0;
    const auto& Siblings = getNode(Parent).Children;
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
// Returns the qualified name of ND. If it is subordinate to Context,
// then the prefix of the latter is removed from the returned value.
std::string SyntaxTree::Impl::getRelativeName(DynTypedNode ND) const {
    // NOTE most of the functionality of this function as clang-specific
    // and not appicable in the generic AST case, so here simple
    // `getName()` call should be sufficient.
    return ND->getName();
}

std::string SyntaxTree::Impl::getNodeValue(NodeId Id) const {
    return getNodeValue(getNode(Id));
}

std::string SyntaxTree::Impl::getNodeValue(const Node& node) const {
    // NOTE similar to the `getRelativeName()`, this function was mostly
    // added in order to provide clang-specific AST handling and is not
    // applicable in the general case
    return node.ASTNode->getName();
}

/// Identifies a node in a subtree by its postorder offset, starting at 1.
struct SNodeId {
    int Id = 0;
    explicit SNodeId(int Id) : Id(Id) {}
    explicit SNodeId() = default;
             operator int() const { return Id; }
    SNodeId& operator++() { return ++Id, *this; }
    SNodeId& operator--() { return --Id, *this; }
    SNodeId  operator+(int Other) const { return SNodeId(Id + Other); }
};
class Subtree {
  private:
    /// The parent tree.
    const SyntaxTree::Impl& Tree;
    /// Maps SNodeIds to original ids.
    std::vector<NodeId> RootIds;
    /// Maps subtree nodes to their leftmost descendants wtihin the
    /// subtree.
    std::vector<SNodeId> LeftMostDescendants;

  public:
    std::vector<SNodeId> KeyRoots;
    Subtree(const SyntaxTree::Impl& Tree, NodeId SubtreeRoot)
        : Tree(Tree) {
        RootIds       = getSubtreePostorder(Tree, SubtreeRoot);
        int NumLeaves = setLeftMostDescendants();
        computeKeyRoots(NumLeaves);
    }
    int    getSize() const { return RootIds.size(); }
    NodeId getIdInRoot(SNodeId Id) const {
        assert(Id > 0 && Id <= getSize() && "Invalid subtree node index.");
        return RootIds[Id - 1];
    }
    const Node& getNode(SNodeId Id) const {
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
    std::string getNodeValue(SNodeId Id) const {
        return Tree.getNodeValue(getIdInRoot(Id));
    }

  private:
    /// Returns the number of leafs in the subtree.
    int setLeftMostDescendants() {
        int NumLeaves = 0;
        LeftMostDescendants.resize(getSize());
        for (int I = 0; I < getSize(); ++I) {
            SNodeId     SI(I + 1);
            const Node& N = getNode(SI);
            NumLeaves += N.isLeaf();
            assert(
                I == Tree.PostorderIds[getIdInRoot(SI)] -
                         getPostorderOffset() &&
                "Postorder traversal in subtree should correspond to "
                "traversal in "
                "the root tree by a constant offset.");
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
/// Computes an optimal mapping between two trees using only insertion,
/// deletion and update as edit actions (similar to the Levenshtein
/// distance).
class ZhangShashaMatcher {
    const ASTDiff::Impl&                         DiffImpl;
    Subtree                                      S1;
    Subtree                                      S2;
    std::unique_ptr<std::unique_ptr<double[]>[]> TreeDist, ForestDist;

  public:
    ZhangShashaMatcher(
        const ASTDiff::Impl&    DiffImpl,
        const SyntaxTree::Impl& T1,
        const SyntaxTree::Impl& T2,
        NodeId                  Id1,
        NodeId                  Id2)
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
                S1.getIdInRoot(Id1), S2.getIdInRoot(Id2)))
            return std::numeric_limits<double>::max();
        return S1.getNodeValue(Id1) != S2.getNodeValue(Id2);
    }
    void computeTreeDist() {
        for (SNodeId Id1 : S1.KeyRoots)
            for (SNodeId Id2 : S2.KeyRoots)
                computeForestDist(Id1, Id2);
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
ASTNodeKind Node::getType() const { return ASTNode->getNodeKind(); }
std::string Node::getTypeLabel() const {
    return "FIXME/HACK"; // getType().asStringRef();
}
std::optional<std::string> Node::getQualifiedIdentifier() const {
    // if (auto* ND = ASTNode.get<NamedDecl>()) {
    //     if (ND->getDeclName().isIdentifier())
    //         return ND->getQualifiedNameAsString();
    // }
    // return llvm::None;
    return "IMPLEMENT";
}

std::optional<std::string> Node::getIdentifier() const {
    // if (auto* ND = ASTNode.get<NamedDecl>()) {
    //     if (ND->getDeclName().isIdentifier()) return ND->getName();
    // }
    // return llvm::None;
    return "IMPLEMENT";
}

// Compares nodes by their depth.
struct HeightLess {
    const SyntaxTree::Impl& Tree;
    HeightLess(const SyntaxTree::Impl& Tree) : Tree(Tree) {}
    bool operator()(NodeId Id1, NodeId Id2) const {
        return Tree.getNode(Id1).Height < Tree.getNode(Id2).Height;
    }
};

// Priority queue for nodes, sorted descendingly by their height.
class PriorityList {
    const SyntaxTree::Impl&                                      Tree;
    HeightLess                                                   Cmp;
    std::vector<NodeId>                                          Container;
    std::priority_queue<NodeId, std::vector<NodeId>, HeightLess> List;

  public:
    PriorityList(const SyntaxTree::Impl& Tree)
        : Tree(Tree), Cmp(Tree), List(Cmp, Container) {}
    void                push(NodeId id) { List.push(id); }
    std::vector<NodeId> pop() {
        int                 Max = peekMax();
        std::vector<NodeId> Result;
        if (Max == 0) return Result;
        while (peekMax() == Max) {
            Result.push_back(List.top());
            List.pop();
        }
        // TODO this is here to get a stable output, not a good heuristic
        std::sort(Result.begin(), Result.end());
        return Result;
    }
    int peekMax() const {
        if (List.empty()) return 0;
        return Tree.getNode(List.top()).Height;
    }
    void open(NodeId Id) {
        for (NodeId Child : Tree.getNode(Id).Children)
            push(Child);
    }
};

bool ASTDiff::Impl::identical(NodeId Id1, NodeId Id2) const {
    const Node& N1 = T1.getNode(Id1);
    const Node& N2 = T2.getNode(Id2);
    if (N1.Children.size() != N2.Children.size() ||
        !isMatchingPossible(Id1, Id2) ||
        T1.getNodeValue(Id1) != T2.getNodeValue(Id2))
        return false;
    for (size_t Id = 0, E = N1.Children.size(); Id < E; ++Id)
        if (!identical(N1.Children[Id], N2.Children[Id])) return false;
    return true;
}
bool ASTDiff::Impl::isMatchingPossible(NodeId Id1, NodeId Id2) const {
    return Options.isMatchingAllowed(T1.getNode(Id1), T2.getNode(Id2));
}
bool ASTDiff::Impl::haveSameParents(
    const Mapping& M,
    NodeId         Id1,
    NodeId         Id2) const {
    NodeId P1 = T1.getNode(Id1).Parent;
    NodeId P2 = T2.getNode(Id2).Parent;
    return (P1.isInvalid() && P2.isInvalid()) ||
           (P1.isValid() && P2.isValid() && M.getDst(P1) == P2);
}
void ASTDiff::Impl::addOptimalMapping(Mapping& M, NodeId Id1, NodeId Id2)
    const {
    if (std::max(
            T1.getNumberOfDescendants(Id1),
            T2.getNumberOfDescendants(Id2)) > Options.MaxSize)
        return;
    ZhangShashaMatcher Matcher(*this, T1, T2, Id1, Id2);
    std::vector<std::pair<NodeId, NodeId>> R = Matcher.getMatchingNodes();
    for (const auto& Tuple : R) {
        NodeId Src = Tuple.first;
        NodeId Dst = Tuple.second;
        if (!M.hasSrc(Src) && !M.hasDst(Dst)) M.link(Src, Dst);
    }
}
double ASTDiff::Impl::getJaccardSimilarity(
    const Mapping& M,
    NodeId         Id1,
    NodeId         Id2) const {
    int         CommonDescendants = 0;
    const Node& N1                = T1.getNode(Id1);
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
    if (Denominator == 0) return 0;
    return CommonDescendants / Denominator;
}
NodeId ASTDiff::Impl::findCandidate(const Mapping& M, NodeId Id1) const {
    NodeId Candidate;
    double HighestSimilarity = 0.0;
    for (NodeId Id2 : T2) {
        if (!isMatchingPossible(Id1, Id2)) continue;
        if (M.hasDst(Id2)) continue;
        double Similarity = getJaccardSimilarity(M, Id1, Id2);
        if (Similarity >= Options.MinSimilarity &&
            Similarity > HighestSimilarity) {
            HighestSimilarity = Similarity;
            Candidate         = Id2;
        }
    }
    return Candidate;
}
void ASTDiff::Impl::matchBottomUp(Mapping& M) const {
    std::vector<NodeId> Postorder = getSubtreePostorder(
        T1, T1.getRootId());
    for (NodeId Id1 : Postorder) {
        if (Id1 == T1.getRootId() && !M.hasSrc(T1.getRootId()) &&
            !M.hasDst(T2.getRootId())) {
            if (isMatchingPossible(T1.getRootId(), T2.getRootId())) {
                M.link(T1.getRootId(), T2.getRootId());
                addOptimalMapping(M, T1.getRootId(), T2.getRootId());
            }
            break;
        }
        bool        Matched         = M.hasSrc(Id1);
        const Node& N1              = T1.getNode(Id1);
        bool        MatchedChildren = std::any_of(
            N1.Children.begin(), N1.Children.end(), [&](NodeId Child) {
                return M.hasSrc(Child);
            });
        if (Matched || !MatchedChildren) continue;
        NodeId Id2 = findCandidate(M, Id1);
        if (Id2.isValid()) {
            M.link(Id1, Id2);
            addOptimalMapping(M, Id1, Id2);
        }
    }
}
Mapping ASTDiff::Impl::matchTopDown() const {
    PriorityList L1(T1);
    PriorityList L2(T2);
    Mapping      M(T1.getSize() + T2.getSize());
    L1.push(T1.getRootId());
    L2.push(T2.getRootId());
    int Max1, Max2;
    while (std::min(Max1 = L1.peekMax(), Max2 = L2.peekMax()) >
           Options.MinHeight) {
        if (Max1 > Max2) {
            for (NodeId Id : L1.pop())
                L1.open(Id);
            continue;
        }
        if (Max2 > Max1) {
            for (NodeId Id : L2.pop())
                L2.open(Id);
            continue;
        }
        std::vector<NodeId> H1, H2;
        H1 = L1.pop();
        H2 = L2.pop();
        for (NodeId Id1 : H1) {
            for (NodeId Id2 : H2) {
                if (identical(Id1, Id2) && !M.hasSrc(Id1) &&
                    !M.hasDst(Id2)) {
                    for (int I = 0, E = T1.getNumberOfDescendants(Id1);
                         I < E;
                         ++I)
                        M.link(Id1 + I, Id2 + I);
                }
            }
        }
        for (NodeId Id1 : H1) {
            if (!M.hasSrc(Id1)) L1.open(Id1);
        }
        for (NodeId Id2 : H2) {
            if (!M.hasDst(Id2)) L2.open(Id2);
        }
    }
    return M;
}
ASTDiff::Impl::Impl(
    SyntaxTree::Impl&        T1,
    SyntaxTree::Impl&        T2,
    const ComparisonOptions& Options)
    : T1(T1), T2(T2), Options(Options) {
    computeMapping();
    computeChangeKinds(TheMapping);
}
void ASTDiff::Impl::computeMapping() {
    TheMapping = matchTopDown();
    if (Options.StopAfterTopDown) return;
    matchBottomUp(TheMapping);
}
void ASTDiff::Impl::computeChangeKinds(Mapping& M) {
    for (NodeId Id1 : T1) {
        if (!M.hasSrc(Id1)) {
            T1.getMutableNode(Id1).Change = Delete;
            T1.getMutableNode(Id1).Shift -= 1;
        }
    }
    for (NodeId Id2 : T2) {
        if (!M.hasDst(Id2)) {
            T2.getMutableNode(Id2).Change = Insert;
            T2.getMutableNode(Id2).Shift -= 1;
        }
    }
    for (NodeId Id1 : T1.NodesBfs) {
        NodeId Id2 = M.getDst(Id1);
        if (Id2.isInvalid()) continue;
        if (!haveSameParents(M, Id1, Id2) ||
            T1.findPositionInParent(Id1, true) !=
                T2.findPositionInParent(Id2, true)) {
            T1.getMutableNode(Id1).Shift -= 1;
            T2.getMutableNode(Id2).Shift -= 1;
        }
    }
    for (NodeId Id2 : T2.NodesBfs) {
        NodeId Id1 = M.getSrc(Id2);
        if (Id1.isInvalid()) continue;
        Node& N1 = T1.getMutableNode(Id1);
        Node& N2 = T2.getMutableNode(Id2);
        if (Id1.isInvalid()) continue;
        if (!haveSameParents(M, Id1, Id2) ||
            T1.findPositionInParent(Id1, true) !=
                T2.findPositionInParent(Id2, true)) {
            N1.Change = N2.Change = Move;
        }
        if (T1.getNodeValue(Id1) != T2.getNodeValue(Id2)) {
            N1.Change = N2.Change = (N1.Change == Move ? UpdateMove : Update);
        }
    }
}
ASTDiff::ASTDiff(
    SyntaxTree&              T1,
    SyntaxTree&              T2,
    const ComparisonOptions& Options)
    : DiffImpl(
          std::make_unique<Impl>(*T1.TreeImpl, *T2.TreeImpl, Options)) {}

ASTDiff::~ASTDiff() = default;

NodeId ASTDiff::getMapped(const SyntaxTree& SourceTree, NodeId Id) const {
    return DiffImpl->getMapped(SourceTree.TreeImpl, Id);
}

SyntaxTree::~SyntaxTree() = default;

const Node& SyntaxTree::getNode(NodeId Id) const {
    return TreeImpl->getNode(Id);
}

int SyntaxTree::getSize() const { return TreeImpl->getSize(); }

NodeId SyntaxTree::getRootId() const { return TreeImpl->getRootId(); }

SyntaxTree::PreorderIterator SyntaxTree::begin() const {
    return TreeImpl->begin();
}

SyntaxTree::PreorderIterator SyntaxTree::end() const {
    return TreeImpl->end();
}

int SyntaxTree::findPositionInParent(NodeId Id) const {
    return TreeImpl->findPositionInParent(Id);
}

std::string SyntaxTree::getNodeValue(NodeId Id) const {
    return TreeImpl->getNodeValue(Id);
}

std::string SyntaxTree::getNodeValue(const Node& N) const {
    return TreeImpl->getNodeValue(N);
}

int main() {
    std::cout << "1\n";
    return 0;
}
