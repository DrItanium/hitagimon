//
// Created by jwscoggins on 6/20/21.
//

#ifndef HITAGIMON_ENVIRONMENTINTERFACE_H
#define HITAGIMON_ENVIRONMENTINTERFACE_H
#include <string>
#include <pair.h>
class EnvironmentInterface
{
public:
    static bool unset(const std::string& name);
    static std::pair<bool, std::string> get(const std::string& name);
    static bool set(const std::string& name, const std::string& value, bool overwrite);
private:
    EnvironmentInterface();
    ~EnvironmentInterface();
    EnvironmentInterface(const EnvironmentInterface&);
    EnvironmentInterface& operator=(const EnvironmentInterface&);
};

#endif //HITAGIMON_ENVIRONMENTINTERFACE_H
