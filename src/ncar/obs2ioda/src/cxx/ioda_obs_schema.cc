#include "ioda_obs_schema.h"

void
IodaObsSchemaComponent::load(const std::shared_ptr<IYamlNode> &node) {
    setNames(node, this->componentType);
}

const std::string &IodaObsSchemaComponent::getValidName() const { return this->validName; }

void
IodaObsSchemaComponent::setNames(const std::shared_ptr<IYamlNode> &node,
                                 const std::string &category) {
    if (node->hasKey(category) && node->isSequence(category)) {
        this->names = node->getStringList(category);
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

std::vector<std::shared_ptr<IYamlNode>>
YamlCppNode::getSequence(const std::string &key) const {
    std::vector<std::shared_ptr<IYamlNode>> result;
    for (const auto &child: node_[key]) {
        result.emplace_back(std::make_shared<YamlCppNode>(child));
    }
    return result;
}

std::vector<std::string>
YamlCppNode::getStringList(const std::string &key) const {
    return node_[key].as<std::vector<std::string>>();
}

bool YamlCppNode::isSequence(const std::string &key) const {
    return node_[key] && node_[key].IsSequence();
}

YamlCppNode::YamlCppNode(YAML::Node node) : node_(std::move(node)) {}

bool YamlCppNode::hasKey(const std::string &key) const {
    return node_[key].IsDefined();
}

IodaObsAttribute::IodaObsAttribute(std::string name)
        : IodaObsSchemaComponent("Attribute", std::move(name)) {}

IodaObsGroup::IodaObsGroup(std::string name)
        : IodaObsSchemaComponent("Group", std::move(name)) {}

IodaObsDimension::IodaObsDimension(std::string name)
        : IodaObsSchemaComponent("Dimension", std::move(name)) {}

IodaObsVariable::IodaObsVariable(std::string name)
        : IodaObsSchemaComponent("Variable", std::move(name)) {}

void IodaObsVariable::load(const std::shared_ptr<IYamlNode> &node) {
    static constexpr std::array<const char *, 2> keys = {"Variable",
                                                         "Dimension"};
    for (const auto &key: keys) {
        if (node->hasKey(key) && node->isSequence(key)) {
            this->setNames(node, key);
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
