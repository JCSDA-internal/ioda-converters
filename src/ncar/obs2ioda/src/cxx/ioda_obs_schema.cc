#include "ioda_obs_schema.h"

IodaObsSchemaComponent::IodaObsSchemaComponent(
    std::string componentType, std::string name
): componentType(std::move(componentType)) {
    if (!name.empty()) {
        this->names.push_back(std::move(name));
        this->validName = this->names.at(0);
    }
}

const std::vector<std::string> &
IodaObsSchemaComponent::getNames() const {
    return this->names;
}

const std::string &IodaObsSchemaComponent::getValidName() const {
    return this->validName;
}

void IodaObsSchemaComponent::load(const YAML::Node &node) {
    setNames(node, this->componentType);
}

void IodaObsSchemaComponent::setNames(
    const YAML::Node &node, const std::string &category
) {
    if (node[category] && node[category].IsSequence()) {
        this->names = node[category].as<std::vector<std::string> >();
    }
    if (!names.empty()) {
        this->validName = this->names.at(0);
    }
}

IodaObsAttribute::IodaObsAttribute(std::string name):
    IodaObsSchemaComponent("Attribute", std::move(name)) {
}

IodaObsGroup::IodaObsGroup(std::string name): IodaObsSchemaComponent(
    "Group", std::move(name)
) {
}

IodaObsDimension::IodaObsDimension(std::string name):
    IodaObsSchemaComponent("Dimension", std::move(name)) {
}

IodaObsVariable::IodaObsVariable(std::string name):
    IodaObsSchemaComponent("Variable", std::move(name)) {
}

void IodaObsVariable::load(const YAML::Node &node) {
    static constexpr std::array<const char *, 2> keys = {
        "Variable", "Dimension"
    };
    for (const auto &key: keys) {
        if (node[key] && node[key].begin() != node[key].end()) {
            this->setNames(node, key);
            break;
        }
    }
}

IodaObsSchema::IodaObsSchema(const YAML::Node &schema) {
    this->loadComponent<IodaObsAttribute>(
        schema, "Attributes", "Attribute", this->attributes
    );
    this->loadComponent<IodaObsGroup>(schema, "Groups", "Group", groups);
    this->loadComponent<IodaObsDimension>(
        schema, "Dimensions", "Dimension", this->dimensions
    );
    this->loadComponent<IodaObsVariable>(
        schema, "Variables", "Variable", this->variables
    );
    this->loadComponent<IodaObsVariable>(
        schema, "Dimensions", "Dimension", this->variables
    );
}

std::shared_ptr<const IodaObsAttribute> IodaObsSchema::getAttribute(
    const std::string &name
) {
    return this->getComponent(name, this->attributes);
}

std::shared_ptr<const IodaObsGroup> IodaObsSchema::getGroup(
    const std::string &name
) {
    return this->getComponent(name, this->groups);
}

std::shared_ptr<const IodaObsDimension> IodaObsSchema::getDimension(
    const std::string &name
) {
    return this->getComponent(name, this->dimensions);
}

std::shared_ptr<const IodaObsVariable> IodaObsSchema::getVariable(
    const std::string &name
) {
    return this->getComponent(name, this->variables);
}
