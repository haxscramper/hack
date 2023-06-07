#include <iostream>
#include <vector>
#include <cassert>

using uint64_t = unsigned long long int;
using uint32_t = unsigned long int;
using uint8_t  = unsigned char;

template <typename T>
using Vec = std::vector<T>;

enum class SemOrgKind : uint8_t
{
    Word,
    Space,
    Subtree
};

struct SemOrg;

struct SemId {
    uint64_t id = 0;
    bool     isNil() const { return id == 0; }

    static SemId Nil() { return SemId(0, SemOrgKind(0), 0); }

    SemId(uint32_t storeIndex, SemOrgKind kind, uint32_t nodeIndex) {
        setStoreIndex(storeIndex);
        setKind(kind);
        setNodeIndex(nodeIndex);
    }

    SemOrgKind getKind() const { return SemOrgKind((id >> 32) & 0xFF); }

    uint32_t getNodeIndex() const {
        assert(!isNil());
        return (id & 0xFFFFFFFF) - 1;
    }

    uint32_t getStoreIndex() const { return id >> 40; }

    void setStoreIndex(uint32_t storeIndex) {
        id = (id & 0x000000FFFFFFFFFF) | ((uint64_t)storeIndex << 40);
    }

    void setKind(SemOrgKind kind) {
        id = (id & 0xFFFFFF00FFFFFFFF) | ((uint64_t)kind << 32);
    }

    void setNodeIndex(uint32_t nodeIndex) {
        id = (id & 0xFFFFFFFF00000000) | (nodeIndex + 1);
    }

    SemOrg& get();
};

template <typename T>
struct SemKindId : public SemId {
    SemId toId() const { return *this; }
    SemKindId(SemId base) : SemId(base) {}

    T* operator->();
};

struct SemOrg {
    virtual SemOrgKind getKind() = 0;
    SemId              parent;
    Vec<SemId>         nested;
    SemOrg(SemId parent) : parent(parent) {}
};

struct Word : SemOrg {
    using SemOrg::SemOrg;
    virtual SemOrgKind getKind() override { return SemOrgKind::Word; }

    static const SemOrgKind staticKind = SemOrgKind::Word;
    static SemKindId<Word>  create(SemId parent);
};

struct Space : SemOrg {
    using SemOrg::SemOrg;
    virtual SemOrgKind getKind() override { return SemOrgKind::Space; }
    static const SemOrgKind staticKind = SemOrgKind::Space;

    static SemKindId<Space> create(SemId parent);
};

struct Subtree : SemOrg {
    using SemOrg::SemOrg;
    virtual SemOrgKind getKind() override { return SemOrgKind::Subtree; }
    static const SemOrgKind   staticKind = SemOrgKind::Subtree;
    static SemKindId<Subtree> create(SemId parent);
};

template <typename T>
struct KindStore {
    Vec<T> values;

    T& getForIndex(uint32_t index) { return values.at(index); }

    SemId create(uint32_t selfIndex, SemId parent) {
        SemId result = SemId(selfIndex, T::staticKind, values.size());
        values.emplace_back(parent);
        return result;
    }
};


struct LocalStore {
    KindStore<Word>    words;
    KindStore<Space>   spaces;
    KindStore<Subtree> subtrees;

    SemOrg& get(SemOrgKind kind, uint32_t index) {
        switch (kind) {
            case SemOrgKind::Word: return words.getForIndex(index);
            case SemOrgKind::Space: return spaces.getForIndex(index);
            case SemOrgKind::Subtree: return subtrees.getForIndex(index);
        }
    }

    SemId create(uint32_t selfIndex, SemOrgKind kind, SemId parent) {
        switch (kind) {
            case SemOrgKind::Word: return words.create(selfIndex, parent);
            case SemOrgKind::Space:
                return spaces.create(selfIndex, parent);
            case SemOrgKind::Subtree:
                return subtrees.create(selfIndex, parent);
        }
    }
};

class GlobalStore {
  public:
    static GlobalStore& getInstance() {
        static GlobalStore instance;
        return instance;
    }

    LocalStore& getStoreByIndex(int index) { return stores.at(index); }

    static SemId createIn(uint32_t index, SemOrgKind kind, SemId parent) {
        return getInstance().getStoreByIndex(index).create(
            index, kind, parent);
    }

    Vec<LocalStore> stores;

  private:
    GlobalStore() {}
    GlobalStore(const GlobalStore&)            = delete;
    GlobalStore& operator=(const GlobalStore&) = delete;
};


SemOrg& SemId::get() {
    return GlobalStore::getInstance()
        .getStoreByIndex(getStoreIndex())
        .get(getKind(), getNodeIndex());
}

template <typename T>
T* SemKindId<T>::operator->() {
    return static_cast<T*>(&get());
}

SemKindId<Space> Space::create(SemId parent) {
    return GlobalStore::createIn(0, SemOrgKind::Space, parent);
}

SemKindId<Word> Word::create(SemId parent) {
    return GlobalStore::createIn(0, SemOrgKind::Word, parent);
}

SemKindId<Subtree> Subtree::create(SemId parent) {
    return GlobalStore::createIn(0, SemOrgKind::Subtree, parent);
}

int main() {
    GlobalStore::getInstance().stores.push_back(LocalStore());
    SemKindId<Subtree> tree = Subtree::create(SemId::Nil());
    SemKindId<Word>    word = Word::create(tree);


    return 0;
}
