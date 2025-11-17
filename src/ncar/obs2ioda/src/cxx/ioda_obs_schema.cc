#include "ioda_obs_schema.h"

void
IodaObsSchemaComponent::load(const std::shared_ptr<IYamlNode> &node,
                             const eckit::LocalConfiguration &config) {
    setNames(node, config, this->componentType);
}

const std::string &IodaObsSchemaComponent::getValidName() const { return this->validName; }

void
IodaObsSchemaComponent::setNames(const std::shared_ptr<IYamlNode> &node,
                                 const eckit::LocalConfiguration &config,
                                 const std::string &key) {
    if (node->hasKey(config, key) && node->isKeySequence(config, key)) {
        this->names = node->getStringList(config, key);
        if (!names.empty()) {
            this->validName = names.at(0);
        }
    }
}

IodaObsSchemaComponent::IodaObsSchemaComponent(
        std::string componentType, std::string name)
        : componentType(std::move(componentType)) {
    if (!name.empty()) {
        this->names.push_back(std::move(name));
        this->validName = this->names.at(0);
    }
}

const std::vector<std::string> &IodaObsSchemaComponent::getNames() const { return this->names; }

// Class YamlEckitNode function definitions
// ******************************************************************************
YamlEckitNode::YamlEckitNode(const std::string& yamlPath)
: node_(eckit::YAMLConfiguration(eckit::PathName(yamlPath))) { }

std::vector<eckit::LocalConfiguration>
YamlEckitNode::getSequence(const std::string &key) const {
     std::vector<eckit::LocalConfiguration> keyConfigs;
     node_.get(key, keyConfigs);
     return keyConfigs;
}

std::vector<std::string>
YamlEckitNode::getStringList(const eckit::LocalConfiguration &config,
                             const std::string &key) const {
    std::vector<std::string> stringVec;
    stringVec = config.getStringVector(key);
    return stringVec;
}

bool YamlEckitNode::isCategorySequence(const std::string &category) const {
    return node_.isList(category);
}

bool YamlEckitNode::isKeySequence(const eckit::LocalConfiguration &config,
                                  const std::string &key) const {
    return config.isList(key);
}

bool YamlEckitNode::hasCategory(const std::string &category) const {
    return node_.has(category);
}

bool YamlEckitNode::hasKey(const eckit::LocalConfiguration &config,
                           const std::string &key) const {
    return config.has(key);
}
// ******************************************************************************

IodaObsAttribute::IodaObsAttribute(std::string name)
        : IodaObsSchemaComponent("Attribute", std::move(name)) {}

IodaObsGroup::IodaObsGroup(std::string name)
        : IodaObsSchemaComponent("Group", std::move(name)) {}

IodaObsDimension::IodaObsDimension(std::string name)
        : IodaObsSchemaComponent("Dimension", std::move(name)) {}

IodaObsVariable::IodaObsVariable(std::string name)
        : IodaObsSchemaComponent("Variable", std::move(name)) {}

void IodaObsVariable::load(const std::shared_ptr<IYamlNode> &node,
                           const eckit::LocalConfiguration &config) {
    static constexpr std::array<const char *, 2> keys = {"Variable",
                                                         "Dimension"};
    for (const auto &key: keys) {
        if (node->hasKey(config, key) && node->isKeySequence(config, key)) {
            this->setNames(node, config, key);
            break;
        }
    }
}

IodaObsSchema::IodaObsSchema(const std::shared_ptr<IYamlNode> &schema) {
    loadComponent<IodaObsAttribute>(schema, "Attributes",
                                    "Attribute", attributes);
    loadComponent<IodaObsGroup>(schema, "Groups", "Group", groups);
    loadComponent<IodaObsDimension>(schema, "Dimensions",
                                    "Dimension", dimensions);
    loadComponent<IodaObsVariable>(schema, "Variables", "Variable",
                                   variables);
    loadComponent<IodaObsVariable>(schema, "Dimensions",
                                   "Dimension",
                                   variables);  // For dimension-as-variable support.
}

std::shared_ptr<const IodaObsAttribute>
IodaObsSchema::getAttribute(const std::string &name) {
    return getComponent(name, attributes);
}

std::shared_ptr<const IodaObsGroup>
IodaObsSchema::getGroup(const std::string &name) {
    return getComponent(name, groups);
}

std::shared_ptr<const IodaObsDimension>
IodaObsSchema::getDimension(const std::string &name) {
    return getComponent(name, dimensions);
}

std::shared_ptr<const IodaObsVariable>
IodaObsSchema::getVariable(const std::string &name) {
    return getComponent(name, variables);
}
