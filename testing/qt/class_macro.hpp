#pragma once

#define DECLARE_MEMBER_GET_SET(MemberType, memberName, functionSuffix)    \
  private:                                                                \
    MemberType memberName;                                                \
                                                                          \
  public:                                                                 \
    void       set##functionSuffix(const MemberType& value);              \
    MemberType get##functionSuffix() const;                               \
                                                                          \
  private:

#define DECLARE_MEMBER_GET_SET_COPY(                                      \
    MemberType, memberName, functionSuffix)                               \
DECLARE_MEMBER_GET_SET((MemberType, memberName, functionSuffix) \
    public: \
 void set##functionSuffix##Copy( MemberType value) ;


#define DEFINE_MEMBER_GET_SET(MemberType, memberName, functionSuffix)     \
  private:                                                                \
    MemberType memberName;                                                \
                                                                          \
  public:                                                                 \
    inline void set##functionSuffix(const MemberType& value) {            \
        memberName = value;                                               \
    }                                                                     \
                                                                          \
    inline MemberType get##functionSuffix() const {                       \
        return memberName;                                                \
    }


#define DEFINE_MEMBER_GET_SET_COPY(                                       \
    MemberType, memberName, functionSuffix)                               \
DEFINE_MEMBER_GET_SET((MemberType, memberName, functionSuffix) \
    public: \
inline void set##functionSuffix##Copy( MemberType value) {            \
    memberName = std::move(value);                                               \
}


#define if_let(__val, __opt)                                              \
    auto __val = typename std::decay_t<decltype(__opt)>::value_type{};    \
    if (__opt.has_value()) {                                              \
        __val = __opt.value();                                            \
    }                                                                     \
                                                                          \
    if (__opt.has_value())


#define for_i(var_name, max_range)                                        \
    for (int var_name = 0; var_name < max_range; ++var_name)


#define for_i_in(var_name, start_value, max_range)                        \
    for (int var_name = start_value; var_name < max_range; ++var_name)
