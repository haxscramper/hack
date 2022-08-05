#ifndef SETTING_FLAGS_HPP
#define SETTING_FLAGS_HPP


/*!
 * \file setting_flags.hpp
 * \brief
 */


//===    STL   ===//
#include <bitset>
#include <iostream>
#include <limits>
#include <map>


//  ////////////////////////////////////


namespace spt {
inline namespace wrappers {
    /*!
     * \brief Convinience class to simplify of class enums as setting flags
     *
     * \todo Overload operator | for two enums to create settings flag.
     * \todo Add intermixing flags that will cancel each other
     * Bitset mask for storing enum flags state
     */
    template <typename Enum>
    struct SettingFlags {
        SettingFlags(const Enum& flag) {
            set_flag(flag, true);
        }

        SettingFlags& operator+=(const Enum& flag) {
            set_flag(flag, true);
            return *this;
        }

        SettingFlags& operator-=(const Enum& flag) {
            set_flag(flag, false);
            return *this;
        }

        SettingFlags& operator=(const Enum& flag) {
            bitset.clear();
            set_flag(flag, true);
            return *this;
        }

        SettingFlags& operator|=(const SettingFlags& other) {
            for (std::pair<const Enum, bool>& flag : bitset) {
                flag.second |= other.test(flag.first);
            }

            return *this;
        }

        SettingFlags& operator&=(const SettingFlags& other) {
            for (Enum& flag : bitset) {
                flag &= other.test(flag);
            }

            return *this;
        }

        bool operator==(const Enum& flag) const {
            return get_flag(flag);
        }

        bool operator!=(const Enum& flag) const {
            return !get_flag(flag);
        }

        bool& operator[](const Enum& flag) {
            return bitset[flag];
        }

        bool test(const Enum& flag) const {
            return get_flag(flag);
        }

        /// \todo replace `auto` with exact template specification
        typename std::map<Enum, bool>::iterator begin() {
            return bitset.begin();
        }

        /// \todo replace `auto` with exact template specification
        typename std::map<Enum, bool>::iterator end() {
            return bitset.end();
        }

        /// \todo replace `auto` with exact template specification
        auto begin() const {
            return bitset.begin();
        }

        /// \todo replace `auto` with exact template specification
        auto end() const {
            return bitset.end();
        }

        void clear() {
            bitset.clear();
        }


      private:
        void set_flag(const Enum& flag, bool val) {
            bitset[flag] = val;
        }

        bool get_flag(const Enum& flag) const {
            auto iter = bitset.find(flag);
            return iter != bitset.end() && (*iter).second;
        }


        using utype = std::underlying_type_t<Enum>;
        std::map<Enum, bool> bitset;
    };

    template <class Enum, class Flag>
    class SettingEnum
    {
      public:
        Flag& operator[](const Enum& key) {
            return map[key];
        }

        bool is_set(const Enum& key) {
            return map.find(key) != map.end();
        }

        void clear() {
            map.clear();
        }


      private:
        std::map<Enum, Flag> map;
    };

} // namespace wrappers
} // namespace spt

#endif // SETTING_FLAGS_HPP
