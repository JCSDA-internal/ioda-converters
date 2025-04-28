#ifndef IODASCHEMA_H
#define IODASCHEMA_H

#include <memory>
#include <string>
#include <unordered_map>
#include <vector>
#include "yaml-cpp/yaml.h"
#include "FilePathConfig.h"



/**
 * @brief Base class for all components of an IODA observation schema.
 *
 * Each component (Variable, Dimension, Attribute, Group) stores a canonical name
 * and a list of deprecated aliases. The canonical name is always the first entry
 * in the `names` vector. Deprecated names follow.
 */
class IodaObsSchemaComponent {
protected:
    std::string validName;
    /**< Canonical (current) name of the component. */
    std::vector<std::string> names;
    /**< First name is canonical; remaining are deprecated. */
    std::string componentType;
    /**< Type of schema component ("Variable", "Attribute", etc.). */

    /**
     * @brief Extracts names from a YAML node and sets the internal name fields.
     *
     * Used when a schema entry contains a list of names for a component, where
     * the first is canonical and the rest are deprecated.
     *
     * @param node The YAML node containing schema definitions.
     * @param category Key used to identify component names (e.g., "Variable").
     */
    void setNames(const YAML::Node &node, const std::string &category);

    /**
     * @brief Constructor for a schema component.
     *
     * @param componentType The component type (e.g., "Variable", "Group").
     * @param name Optional single name, used as the canonical name if provided.
     */
    explicit IodaObsSchemaComponent(
        std::string componentType, std::string name = ""
    );

public:
    /**
     * @brief Returns the canonical (valid) name of the component.
     *
     * @return Reference to the primary name.
     */
    [[nodiscard]] const std::string &getValidName() const;

    /**
     * @brief Returns all known names for the component.
     *
     * The first entry is the canonical name. All others are deprecated aliases.
     *
     * @return Reference to the list of names.
     */
    [[nodiscard]] const std::vector<std::string> &getNames() const;

    /**
     * @brief Loads the component from a YAML node.
     *
     * By default, this sets the name(s) based on the component type.
     *
     * @param node The YAML node describing the component.
     */
    virtual void load(const YAML::Node &node);

    /**
     * @brief Virtual destructor.
     */
    virtual ~IodaObsSchemaComponent() = default;
};

/**
 * @brief Represents an Attribute component in the IODA schema.
 */
class IodaObsAttribute final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for an attribute component.
     * @param name Optional name used as the canonical name.
     */
    explicit IodaObsAttribute(std::string name = "");
};

/**
 * @brief Represents a Group component in the IODA schema.
 */
class IodaObsGroup final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for a group component.
     * @param name Optional name used as the canonical name.
     */
    explicit IodaObsGroup(std::string name = "");
};

/**
 * @brief Represents a Dimension component in the IODA schema.
 */
class IodaObsDimension final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for a dimension component.
     * @param name Optional name used as the canonical name.
     */
    explicit IodaObsDimension(std::string name = "");
};

/**
 * @brief Represents a Variable component in the IODA schema.
 */
class IodaObsVariable final : public IodaObsSchemaComponent {
public:
    /**
     * @brief Constructor for a variable component.
     * @param name Optional name used as the canonical name.
     */
    explicit IodaObsVariable(std::string name = "");

    /**
     * @brief Loads the variable definition from a YAML node.
     *
     * A variable may be defined under either a "Variable" or a "Dimension" node
     * in the YAML schema. This method checks both keys in order to support
     * dimension variables that are defined globally.
     *
     * This is necessary because every dimension is also represented as a global
     * variable in IODA files (e.g., `/nlocs`), and must be accessible under both
     * schema categories.
     *
     * @param node The YAML node describing the variable or dimension.
     */
    void load(const YAML::Node &node) override;
};

/**
 * @brief Parses and manages the full IODA observation schema.
 *
 * This class loads the schema from a YAML document and manages
 * collections of variables, dimensions, groups, and attributes.
 * Deprecated aliases are automatically recognized and mapped to
 * the correct canonical name.
 */
class IodaObsSchema {
    std::unordered_map<std::string, std::shared_ptr<IodaObsVariable> >
    variables;
    std::unordered_map<std::string, std::shared_ptr<IodaObsDimension> >
    dimensions;
    std::unordered_map<std::string, std::shared_ptr<IodaObsGroup> >
    groups;
    std::unordered_map<std::string, std::shared_ptr<IodaObsAttribute> >
    attributes;

    /**
     * @brief Loads a specific component category (e.g., Variables) from the schema.
     *
     * Each item in the YAML sequence is loaded and registered under all its names.
     *
     * @tparam T Component type (e.g., IodaObsVariable).
     * @param schema YAML node containing the full schema.
     * @param category The YAML key for the component type (e.g., "Variables").
     * @param key The name used to find aliases inside each item.
     * @param componentMap Storage for created components.
     */
    template<typename T> void loadComponent(
        const YAML::Node &schema, const std::string &category,
        const std::string &key,
        std::unordered_map<std::string, std::shared_ptr<T> > &
        componentMap
    ) {
        if (schema[category] && schema[category].IsSequence()) {
            for (const auto &item: schema[category]) {
                if (item[key]) {
                    auto component = std::make_shared<T>();
                    component->load(item);
                    for (const auto &componentName: component->
                         getNames()) {
                        componentMap.emplace(componentName, component);
                    }
                }
            }
        }
    }

    /**
     * @brief Looks up a component by name or creates a new one.
     *
     * If a component is not already loaded, a placeholder with the given name
     * is created and inserted into the map.
     *
     * @tparam T Component type.
     * @param name Name or alias of the component.
     * @param componentMap Map from name to shared component.
     * @return Shared pointer to the component.
     */
    template<typename T> std::shared_ptr<const T> getComponent(
        const std::string &name,
        std::unordered_map<std::string, std::shared_ptr<T> > &
        componentMap
    ) {
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
     * @brief Constructs a schema object and loads from a parsed YAML node.
     * @param schema Root node of a parsed IODA schema file.
     */
    explicit IodaObsSchema(const YAML::Node &schema);

    /**
     * @brief Gets an attribute by name or deprecated alias.
     * @param name Name or deprecated name of the attribute.
     * @return Shared pointer to the attribute.
     */
    std::shared_ptr<const IodaObsAttribute> getAttribute(
        const std::string &name
    );

    /**
     * @brief Gets a dimension by name or deprecated alias.
     * @param name Name or deprecated name of the dimension.
     * @return Shared pointer to the dimension.
     */
    std::shared_ptr<const IodaObsDimension> getDimension(
        const std::string &name
    );

    /**
     * @brief Gets a group by name or deprecated alias.
     * @param name Name or deprecated name of the group.
     * @return Shared pointer to the group.
     */
    std::shared_ptr<const IodaObsGroup> getGroup(
        const std::string &name
    );

    /**
     * @brief Gets a variable by name or deprecated alias.
     * @param name Name or deprecated name of the variable.
     * @return Shared pointer to the variable.
     */
    std::shared_ptr<const IodaObsVariable> getVariable(
        const std::string &name
    );
};

#endif // IODASCHEMA_H
