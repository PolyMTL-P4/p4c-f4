/*
Copyright 2013-present Barefoot Networks, Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/
#ifndef _LIB_ERROR_HELPER_H_
#define _LIB_ERROR_HELPER_H_

#include <fmt/args.h>
#include <fmt/core.h>

#include <cstdarg>
#include <map>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <vector>

#include "lib/cstring.h"
#include "lib/error_message.h"
#include "lib/source_file.h"
#include "lib/stringify.h"

class FmtFormatter {
    const char *format;
    std::vector<const char *> args;
    fmt::dynamic_format_arg_store<fmt::format_context> store;

 public:
    explicit FmtFormatter(const char *format) : format(format) {}

    [[nodiscard]] std::string str() const { return fmt::vformat(format, store); }
    FmtFormatter &addArg(const char *arg) {
        store.push_back(arg);
        return *this;
    }
    FmtFormatter &addArg(const std::string &arg) {
        store.push_back(arg.c_str());
        return *this;
    }
    template <typename T,
              typename std::enable_if_t<Util::hasToString<T>(), std::nullptr_t> = nullptr>
    FmtFormatter &addArg(const T *arg) {
        args.push_back(arg->toString().c_str());
        return *this;
    }
    template <typename T,
              typename std::enable_if_t<Util::hasToString<T>(), std::nullptr_t> = nullptr>
    FmtFormatter &addArg(const T &arg) {
        args.push_back(arg.toString().c_str());
        return *this;
    }
    template <typename T,
              typename std::enable_if_t<std::is_arithmetic_v<T>, std::nullptr_t> = nullptr>
    FmtFormatter &addArg(const T &arg) {
        args.push_back(std::to_string(arg).c_str());
        return *this;
    }
};

namespace priv {
// All these methods return std::string because this is the native format of FmtFormatter
// Position is printed at the beginning.
static inline ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out) {
    out.message = f.str();
    return out;
}

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const char *t, Args... args);

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const cstring &t, Args... args);

// use: ir/mau.cpp:805
template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const Util::SourceInfo &info,
                          Args... args);

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T &t, Args... args) ->
    typename std::enable_if<Util::hasToString<T>() &&
                                !std::is_base_of<Util::IHasSourceInfo, T>::value,
                            ErrorMessage>::type;

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T *t, Args... args) ->
    typename std::enable_if<Util::hasToString<T>() &&
                                !std::is_base_of<Util::IHasSourceInfo, T>::value,
                            ErrorMessage>::type;

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T &t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, ErrorMessage>::type;

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T *t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, ErrorMessage>::type;

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const big_int *t, Args... args);

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const big_int &t, Args... args);

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T &t, Args... args) ->
    typename std::enable_if<std::is_arithmetic<T>::value, ErrorMessage>::type;

// actual implementations

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const char *t, Args... args) {
    return error_helper(f.addArg(t), out, std::forward<Args>(args)...);
}

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const cstring &t, Args... args) {
    return error_helper(f.addArg(t.c_str()), out, std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T &t, Args... args) ->
    typename std::enable_if<Util::hasToString<T>() &&
                                !std::is_base_of<Util::IHasSourceInfo, T>::value,
                            ErrorMessage>::type {
    return error_helper(f.addArg(t.toString()), out, std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T *t, Args... args) ->
    typename std::enable_if<Util::hasToString<T>() &&
                                !std::is_base_of<Util::IHasSourceInfo, T>::value,
                            ErrorMessage>::type {
    return error_helper(f.addArg(t->toString()), out, std::forward<Args>(args)...);
}

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const big_int *t, Args... args) {
    return error_helper(f.addArg(t->str().c_str()), out, std::forward<Args>(args)...);
}

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const big_int &t, Args... args) {
    return error_helper(f.addArg(t.str().c_str()), out, std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T &t, Args... args) ->
    typename std::enable_if<std::is_arithmetic<T>::value, ErrorMessage>::type {
    return error_helper(f.addArg(t), out, std::forward<Args>(args)...);
}

template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage out, const Util::SourceInfo &info,
                          Args... args) {
    if (info.isValid()) out.locations.push_back(info);
    return error_helper(f.addArg(""), out, std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T *t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, ErrorMessage>::type {
    auto info = t->getSourceInfo();
    if (info.isValid()) out.locations.push_back(info);
    return error_helper(f.addArg(t->toString()), out, std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto error_helper(FmtFormatter &f, ErrorMessage out, const T &t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, ErrorMessage>::type {
    auto info = t.getSourceInfo();
    if (info.isValid()) out.locations.push_back(info);
    return error_helper(f.addArg(t.toString()), out, std::forward<Args>(args)...);
}

}  // namespace priv

// Most direct invocations of error_helper usually only reduce arguments
template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, Args... args) {
    ErrorMessage msg;
    return ::priv::error_helper(f, msg, std::forward<Args>(args)...);
}

// Invoked from ErrorReporter
template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, ErrorMessage &msg, Args... args) {
    return ::priv::error_helper(f, msg, std::forward<Args>(args)...);
}

// This overload exists for backwards compatibility
template <class... Args>
ErrorMessage error_helper(FmtFormatter &f, const std::string &prefix, const Util::SourceInfo &info,
                          const std::string &suffix, Args... args) {
    ErrorMessage msg(prefix, info, suffix);
    return ::priv::error_helper(f, msg, std::forward<Args>(args)...);
}

/***********************************************************************************/

static inline std::string bug_helper(FmtFormatter &f, std::string message, std::string position,
                                     std::string tail) {
    std::string text = f.str();
    std::string result = position;
    if (!position.empty()) result += ": ";
    result += message + text + "\n" + tail;
    return result;
}

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const char *t, Args... args);

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const cstring &t, Args... args);

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const Util::SourceInfo &info, Args... args);

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T &t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type;

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T *t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type;

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T *t, Args... args) ->
    typename std::enable_if<!std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type;

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const big_int *t, Args... args);

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const big_int &t, Args... args);

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T &t, Args... args) ->
    typename std::enable_if<!std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type;

// actual implementations

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const char *t, Args... args) {
    return bug_helper(f.addArg(t), message, position, tail, std::forward<Args>(args)...);
}

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const cstring &t, Args... args) {
    return bug_helper(f.addArg(t.c_str()), message, position, tail, std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T *t, Args... args) ->
    typename std::enable_if<!std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type {
    std::stringstream str;
    str << t;
    return bug_helper(f.addArg(str.str().c_str()), message, position, tail,
                      std::forward<Args>(args)...);
}

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const big_int *t, Args... args) {
    return bug_helper(f.addArg(t->str().c_str()), message, position, tail,
                      std::forward<Args>(args)...);
}

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const big_int &t, Args... args) {
    return bug_helper(f.addArg(t.str().c_str()), message, position, tail,
                      std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T &t, Args... args) ->
    typename std::enable_if<!std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type {
    return bug_helper(f.addArg(t), message, position, tail, std::forward<Args>(args)...);
}

template <class... Args>
std::string bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                       const Util::SourceInfo &info, Args... args) {
    cstring posString = info.toPositionString();
    if (position.empty()) {
        position = posString;
        posString = "";
    } else {
        if (!posString.isNullOrEmpty()) {
            posString += "\n";
        }
    }
    return bug_helper(f.addArg(""), message, position, tail + posString + info.toSourceFragment(),
                      std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T *t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type {
    if (t == nullptr) {
        return bug_helper(f, message, position, tail, std::forward<Args>(args)...);
    }

    cstring posString = t->getSourceInfo().toPositionString();
    if (position.empty()) {
        position = posString;
        posString = "";
    } else {
        if (!posString.isNullOrEmpty()) {
            posString += "\n";
        }
    }
    std::stringstream str;
    str << t;
    return bug_helper(f.addArg(str.str().c_str()), message, position,
                      tail + posString + t->getSourceInfo().toSourceFragment(),
                      std::forward<Args>(args)...);
}

template <typename T, class... Args>
auto bug_helper(FmtFormatter &f, std::string message, std::string position, std::string tail,
                const T &t, Args... args) ->
    typename std::enable_if<std::is_base_of<Util::IHasSourceInfo, T>::value, std::string>::type {
    cstring posString = t.getSourceInfo().toPositionString();
    if (position.empty()) {
        position = posString;
        posString = "";
    } else {
        if (!posString.isNullOrEmpty()) {
            posString += "\n";
        }
    }
    std::stringstream str;
    str << t;
    return bug_helper(f.addArg(str.str().c_str()), message, position,
                      tail + posString + t.getSourceInfo().toSourceFragment(),
                      std::forward<Args>(args)...);
}

#endif  // _LIB_ERROR_HELPER_H_
