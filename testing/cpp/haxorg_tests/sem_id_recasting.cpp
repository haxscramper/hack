#include <iostream>
#include <vector>
#include <cassert>

using uint64_t = unsigned long long int;
using uint32_t = unsigned long int;
using uint8_t  = unsigned char;

template <typename T>
using Vec = std::vector<T>;

enum class OrgKind : uint8_t
{
    Word,
    Space,
    Subtree
};

struct Org;

struct OrgId {
    uint64_t id = 0;
    bool     isNil() const { return id == 0; }

    static OrgId Nil() { return OrgId(0, OrgKind(0), 0); }

    OrgId(uint32_t storeIndex, OrgKind kind, uint32_t nodeIndex) {
        setStoreIndex(storeIndex);
        setKind(kind);
        setNodeIndex(nodeIndex);
    }

    OrgKind getKind() const { return OrgKind((id >> 32) & 0xFF); }

    uint32_t getNodeIndex() const {
        assert(!isNil());
        return (id & 0xFFFFFFFF) - 1;
    }

    uint32_t getStoreIndex() const { return id >> 40; }

    void setStoreIndex(uint32_t storeIndex) {
        id = (id & 0x000000FFFFFFFFFF) | ((uint64_t)storeIndex << 40);
    }

    void setKind(OrgKind kind) {
        id = (id & 0xFFFFFF00FFFFFFFF) | ((uint64_t)kind << 32);
    }

    void setNodeIndex(uint32_t nodeIndex) {
        id = (id & 0xFFFFFFFF00000000) | (nodeIndex + 1);
    }

    Org& get();
};

template <typename T>
struct SemKindId : public OrgId {
    OrgId toId() const { return *this; }
    SemKindId(OrgId base) : OrgId(base) {}

    T* operator->();
};

struct Org {
    virtual OrgKind getKind() = 0;
    OrgId           parent;
    Vec<OrgId>      nested;
    Org(OrgId parent) : parent(parent) {}
    void  push_back(OrgId sub) { nested.push_back(sub); }
    OrgId at(int index) { return nested.at(index); }
};

struct Word : Org {
    using Org::Org;
    virtual OrgKind getKind() override { return OrgKind::Word; }

    static const OrgKind   staticKind = OrgKind::Word;
    static SemKindId<Word> create(OrgId parent);
};

struct Space : Org {
    using Org::Org;
    virtual OrgKind      getKind() override { return OrgKind::Space; }
    static const OrgKind staticKind = OrgKind::Space;

    static SemKindId<Space> create(OrgId parent);
};

struct Subtree : Org {
    using Org::Org;
    virtual OrgKind      getKind() override { return OrgKind::Subtree; }
    static const OrgKind staticKind = OrgKind::Subtree;
    static SemKindId<Subtree> create(OrgId parent);
};

template <typename T>
struct KindStore {
    Vec<T> values;

    T& getForIndex(uint32_t index) { return values.at(index); }

    OrgId create(uint32_t selfIndex, OrgId parent) {
        OrgId result = OrgId(selfIndex, T::staticKind, values.size());
        values.emplace_back(parent);
        return result;
    }
};


struct LocalStore {
    KindStore<Word>    words;
    KindStore<Space>   spaces;
    KindStore<Subtree> subtrees;

    Org& get(OrgKind kind, uint32_t index) {
        switch (kind) {
            case OrgKind::Word: return words.getForIndex(index);
            case OrgKind::Space: return spaces.getForIndex(index);
            case OrgKind::Subtree: return subtrees.getForIndex(index);
        }
    }

    OrgId create(uint32_t selfIndex, OrgKind kind, OrgId parent) {
        switch (kind) {
            case OrgKind::Word: return words.create(selfIndex, parent);
            case OrgKind::Space: return spaces.create(selfIndex, parent);
            case OrgKind::Subtree:
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

    static OrgId createIn(uint32_t index, OrgKind kind, OrgId parent) {
        return getInstance().getStoreByIndex(index).create(
            index, kind, parent);
    }

    Vec<LocalStore> stores;

  private:
    GlobalStore() {}
    GlobalStore(const GlobalStore&)            = delete;
    GlobalStore& operator=(const GlobalStore&) = delete;
};


Org& OrgId::get() {
    return GlobalStore::getInstance()
        .getStoreByIndex(getStoreIndex())
        .get(getKind(), getNodeIndex());
}

template <typename T>
T* SemKindId<T>::operator->() {
    return static_cast<T*>(&get());
}

SemKindId<Space> Space::create(OrgId parent) {
    return GlobalStore::createIn(0, OrgKind::Space, parent);
}

SemKindId<Word> Word::create(OrgId parent) {
    return GlobalStore::createIn(0, OrgKind::Word, parent);
}

SemKindId<Subtree> Subtree::create(OrgId parent) {
    return GlobalStore::createIn(0, OrgKind::Subtree, parent);
}

int main() {
    GlobalStore::getInstance().stores.push_back(LocalStore());
    SemKindId<Subtree> tree = Subtree::create(OrgId::Nil());
    SemKindId<Word>    word = Word::create(tree);


    std::cout << "ok\n";
    return 0;
}
