#ifndef IODASCHEMA_H
#define IODASCHEMA_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include <array>
#include <utility>

#include "yaml-cpp/yaml.h"
#include "FilePathConfig.h"

/**
 * @brief Interface for generic YAML node access.
 *
 * This abstraction allows schema parsing code to be independent of any specific
 * YAML library (e.g., yaml-cpp). It enables dependency injection and mocking.
 */
class IYamlNode {
public:
    /**
     * @brief Check whether a key exists in the node.
     * @param key The key to check.
     * @return True if the key exists and is defined.
     */
    [[nodiscard]] virtual bool hasKey(const std::string &key) const = 0;

    /**
     * @brief Check whether the specified key holds a YAML sequence.
     * @param key The key to check.
     * @return True if the key maps to a sequence node.
     */
    [[nodiscard]] virtual bool
    isSequence(const std::string &key) const = 0;

    /**
     * @brief Retrieve a list of strings from a YAML sequence under a key.
     * @param key The key for the sequence.
     * @return A vector of strings.
     */
    [[nodiscard]] virtual std::vector<std::string>
    getStringList(const std::string &key) const = 0;

    /**
     * @brief Retrieve a list of child nodes from a YAML sequence under a key.
     * @param key The key for the sequence.
     * @return A vector of shared pointers to IYamlNode children.
     */
    [[nodiscard]] virtual std::vector<std::shared_ptr<IYamlNode>>
    getSequence(const std::string &key) const = 0;

    /// @brief Virtual destructor.
    virtual ~IYamlNode() = default;
};

/**
 * @brief Concrete implementation of IYamlNode using yaml-cpp.
 *
 * Wraps a `YAML::Node` object and implements the `IYamlNode` interface to provide
 * generic access to YAML keys, sequences, and nested nodes.
 */
class YamlCppNode : public IYamlNode {
public:
    /**
     * @brief Constructor from a yaml-cpp node.
     * @param node The YAML::Node to wrap.
     */
    explicit YamlCppNode(YAML::Node node);

    /**
     * @brief Check if a key exists and is defined in the wrapped node.
     * @param key The key to check.
     * @return True if the key exists and is defined.
     */
    bool hasKey(const std::string &key) const override;

    /**
     * @brief Check whether the value at a key is a YAML sequence.
     * @param key The key to check.
     * @return True if the value is a sequence.
     */
    bool isSequence(const std::string &key) const override;

    /**
     * @brief Extract a vector of strings from a sequence under a given key.
     * @param key The YAML key containing the string list.
     * @return A vector of strings.
     */
    std::vector<std::string>
    getStringList(const std::string &key) const override;

    /**
     * @brief Extract a list of child nodes from a sequence under a key.
     * @param key The key containing the YAML sequence.
     * @return A vector of wrapped child IYamlNode instances.
     */
    std::vector<std::shared_ptr<IYamlNode>>
    getSequence(const std::string &key) const override;

private:
    YAML::Node node_;  ///< The wrapped yaml-cpp node.
};


/**
 * @brief Abstract base class for all IODA schema components.
 *
 * Each component (e.g., Variable, Attribute) has a canonical name and
 * optionally a list of deprecated aliases.
 */
class IodaObsSchemaComponent {
protected:
    std::string validName;              ///< Canonical name (first name in list).
    std::vector<std::string> names;     ///< All known names (canonical + aliases).
    std::string componentType;          ///< Type: "Variable", "Attribute", etc.

    /**
     * @brief Set names and canonical name from a YAML node.
     *
     * This method extracts a list of names under a given category key
     * and sets the first one as the canonical name.
     *
     * @param node The YAML node containing the list.
     * @param category The key to look up (e.g., "Variable", "Attribute").
     */
    void setNames(const std::shared_ptr<IYamlNode> &node,
                  const std::string &category);

    /**
     * @brief Constructor.
     * @param componentType Name of the component type.
     * @param name Optional canonical name.
     */
    explicit IodaObsSchemaComponent(std::string componentType,
                                    std::string name = "");

public:
    /**
     * @brief Returns the canonical name of the component.
     * @return Canonical name string.
     */
    [[nodiscard]] const std::string &
    getValidName() const;

    /**
     * @brief Returns all names (canonical + deprecated).
     * @return Vector of all known names.
     */
    [[nodiscard]] const std::vector<std::string> &
    getNames() const;

    /**
     * @brief Load the component metadata from a YAML node.
     * @param node YAML node describing the component.
     */
    virtual void load(const std::shared_ptr<IYamlNode> &node);

    /// @brief Virtual destructor.
    virtual ~IodaObsSchemaComponent() = default;
};

/**
 * @brief Represents an attribute entry in the schema.
 */
class IodaObsAttribute final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for an attribute component.
     * @param name Optional attribute name.
     */
    explicit IodaObsAttribute(std::string name = "");
};

/**
 * @brief Represents a group entry in the schema.
 */
class IodaObsGroup final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for a group component.
     * @param name Optional group name.
     */
    explicit IodaObsGroup(std::string name = "");
};

/**
 * @brief Represents a dimension entry in the schema.
 */
class IodaObsDimension final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for a dimension component.
     * @param name Optional dimension name.
     */
    explicit IodaObsDimension(std::string name = "");
};

/**
 * @brief Represents a variable entry in the schema.
 *
 * Variables may appear under both "Variables" and "Dimensions".
 */
class IodaObsVariable final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for a variable component.
     * @param name Optional variable name.
     */
    explicit IodaObsVariable(std::string name = "");

    /**
     * @brief Load variable metadata from the provided YAML node.
     * @param node YAML node containing variable data.
     */
    void load(const std::shared_ptr<IYamlNode> &node) override;
};

/**
 * @brief Full schema loader and manager.
 *
 * Parses schema components and resolves deprecated names to canonical ones.
 */
class IodaObsSchema {
    std::unordered_map<std::string, std::shared_ptr<IodaObsVariable>> variables;   ///< Variable name to variable object.
    std::unordered_map<std::string, std::shared_ptr<IodaObsDimension>> dimensions; ///< Dimension name to dimension object.
    std::unordered_map<std::string, std::shared_ptr<IodaObsGroup>> groups;         ///< Group name to group object.
    std::unordered_map<std::string, std::shared_ptr<IodaObsAttribute>> attributes; ///< Attribute name to attribute object.

    /**
     * @brief Generic component loader from YAML into the component map.
     *
     * @tparam T Component type.
     * @param schema Root YAML node.
     * @param category YAML key for this component type (e.g., "Variables").
     * @param key Component-specific key (e.g., "Variable").
     * @param componentMap Output map to populate.
     */
    template<typename T>
    void loadComponent(const std::shared_ptr<IYamlNode> &schema,
                       const std::string &category,
                       const std::string &key,
                       std::unordered_map<std::string, std::shared_ptr<T>> &componentMap) {
        if (schema->hasKey(category) && schema->isSequence(category)) {
            for (const auto &item: schema->getSequence(category)) {
                if (item->hasKey(key)) {
                    auto component = std::make_shared<T>();
                    component->load(item);
                    for (const auto &n: component->getNames()) {
                        componentMap.emplace(n, component);
                    }
                }
            }
        }
    }

    /**
     * @brief Lookup or create a schema component.
     *
     * @tparam T Component type.
     * @param name Name of the component to look up.
     * @param componentMap Component map to search.
     * @return Shared pointer to the component.
     */
    template<typename T>
    std::shared_ptr<const T> getComponent(const std::string &name,
                                          std::unordered_map<std::string, std::shared_ptr<T>> &componentMap) {
        auto it = componentMap.find(name);
        if (it != componentMap.end()) {
            return it->second;
        }
        auto component = std::make_shared<T>(name);
        componentMap[name] = component;
        return component;
    }

public:
    /**
     * @brief Construct and populate schema from YAML node.
     * @param schema Shared pointer to parsed YAML root node.
     */
    explicit IodaObsSchema(const std::shared_ptr<IYamlNode> &schema);

    /**
     * @brief Retrieve an attribute by name.
     * @param name Canonical or alias name.
     * @return Shared pointer to attribute object.
     */
    std::shared_ptr<const IodaObsAttribute>
    getAttribute(const std::string &name);

    /**
     * @brief Retrieve a group by name.
     * @param name Canonical or alias name.
     * @return Shared pointer to group object.
     */
    std::shared_ptr<const IodaObsGroup>
    getGroup(const std::string &name);

    /**
     * @brief Retrieve a dimension by name.
     * @param name Canonical or alias name.
     * @return Shared pointer to dimension object.
     */
    std::shared_ptr<const IodaObsDimension>
    getDimension(const std::string &name);

    /**
     * @brief Retrieve a variable by name.
     * @param name Canonical or alias name.
     * @return Shared pointer to variable object.
     */
    std::shared_ptr<const IodaObsVariable>
    getVariable(const std::string &name);
};

#endif  // IODASCHEMA_H
