#!/usr/bin/env python

from beartype.typing import List, Dict, Any, Union, Optional, Literal
from typing import Annotated, get_args
from pydantic import BaseModel, Field, field_validator, Discriminator, ValidationError, AliasChoices, ConfigDict, model_validator
from abc import ABC, abstractmethod
import json
from pathlib import Path


class FluidModel(BaseModel):
    amount: Optional[int] = None
    fluid: Optional[str] = None
    tag: Optional[str] = None
    class Config:
        extra = "forbid"

    @property
    def modname(self) -> Optional[str]:
        if isinstance(self.fluid, str):
            return self.fluid.split(":")[0]

    @property
    def itemname(self) -> Optional[str]:
        if isinstance(self.fluid, str):
            return self.fluid.split(":")[1]

class FluidInput(FluidModel):
    pass


class FluidOutput(FluidModel):
    pass


class ItemModel(BaseModel):
    type: Optional[str] = None
    amount: Optional[Union[int, Any]] = Field(default=None,
                                              validation_alias=AliasChoices(
                                                  "amount", "count"))
    item: Optional[Union[str, Any]] = Field(default=None,
                                            validation_alias=AliasChoices(
                                                "item", "id"))

    @property
    def modname(self) -> Optional[str]:
        if isinstance(self.item, str):
            return self.item.split(":")[0]

    @property
    def itemname(self) -> Optional[str]:
        if isinstance(self.item, str):
            return self.item.split(":")[1]

    @model_validator(mode="before")
    @classmethod
    def normalize_item_field(cls, data):
        if isinstance(data, dict):
            if ("item" in data and isinstance(data["item"], dict)) or ("id" in data and isinstance(data["id"], dict)):
                if "id" in data["item"]:
                    data["item"] = data["item"]["id"]

            if "tag" in data and isinstance(data["tag"], dict) and "tag" in data["tag"]:
                data["tag"] = data["tag"]["tag"]

        return data

    tag: Optional[Union[str, Any]] = None
    probability: Optional[float] = None

    base: Optional["ItemModel"] = None
    subtracted: Optional["ItemModel"] = None
    children: Optional[List["ItemModel"]] = None

    class Config:
        extra = "forbid"

class ItemInput(ItemModel):
    pass


class ItemOutput(ItemModel):
    pass


ItemInputUse = Union[ItemInput, List[ItemInput]]
ItemOutputUse = Union[ItemOutput, List[ItemOutput]]


class NeoforgeCondition(BaseModel):
    type: str
    model_config = ConfigDict(extra='allow')


class Recipe(BaseModel, ABC):
    type: str
    category: Optional[str] = None
    group: Optional[str] = None

    @field_validator("type")
    @classmethod
    def parse_type(cls, v: str) -> str:
        return v

    class Config:
        extra = "allow"


class MIElectricRecipe(Recipe):
    duration: int
    eu: int
    fluid_inputs: Optional[List[FluidInput]] = None
    fluid_outputs: Optional[List[FluidOutput]] = None
    item_inputs: Optional[List[ItemInput]] = None
    item_outputs: Optional[List[ItemOutput]] = None

    @staticmethod
    def normalize_to_list(v):
        if v is None:
            return None
        if isinstance(v, list):
            return v
        return [v]

    @field_validator("item_inputs", mode="before")
    @classmethod
    def normalize_item_inputs(cls, v):
        return MIElectricRecipe.normalize_to_list(v)

    @field_validator("fluid_inputs", mode="before")
    @classmethod
    def normalize_fluid_inputs(cls, v):
        return MIElectricRecipe.normalize_to_list(v)

    @field_validator("item_outputs", mode="before")
    @classmethod
    def normalize_item_outputs(cls, v):
        return MIElectricRecipe.normalize_to_list(v)

    @field_validator("fluid_outputs", mode="before")
    @classmethod
    def normalize_fluid_outputs(cls, v):
        return MIElectricRecipe.normalize_to_list(v)


class ChemicalReactorRecipe(MIElectricRecipe):
    type: Literal[r"modern_industrialization:chemical_reactor"]


class CraftingShapedRecipe(Recipe):
    type: Literal[r"minecraft:crafting_shaped"]
    key: Dict[str, ItemInputUse]
    pattern: List[str]
    result: ItemOutputUse


class PackerRecipe(MIElectricRecipe):
    type: Literal[r"modern_industrialization:packer"]


class ConditionalItemOutput(BaseModel):
    item: str
    amount: int
    probability: float


class ProbabilisticItemInput(BaseModel):
    item: str
    amount: int
    probability: float


class AssemblerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:assembler"]


class StonecuttingRecipe(Recipe):
    type: Literal["minecraft:stonecutting"]
    ingredient: ItemInputUse
    result: ItemOutputUse


class CraftingShapelessRecipe(Recipe):
    type: Literal["minecraft:crafting_shapeless"]
    ingredients: List[Union[ItemInput, List[ItemInput]]]
    result: ItemOutputUse
    group: Optional[str] = None


class NeoforgeCondition(BaseModel):
    type: str


class MillingRecipe(Recipe):
    type: Literal["create:milling"]
    ingredients: List[ItemInput]
    processing_time: int
    results: List[ItemOutput]


class ForgeHammerRecipe(Recipe):
    type: Literal["modern_industrialization:forge_hammer"]
    damage: Optional[int] = None
    ingredient: ItemInputUse
    result: ItemOutputUse
    count: Optional[int] = None


class CuttingRecipe(Recipe):
    type: Literal["create:cutting"]
    ingredients: List[ItemInput]
    processing_time: Optional[int] = None
    results: List[ItemOutput]


class QuarryRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:quarry"]


class MixerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:mixer"]


class XycraftBlockRule(BaseModel):
    block: Optional[str] = None
    predicate_type: Literal["xycraft_core:block_rule"]


class XycraftFluidTypeRule(BaseModel):
    fluid_type: Optional[str] = None
    predicate_type: Literal["xycraft_core:fluid_type_rule"]


XycraftRule = Union[XycraftBlockRule, XycraftFluidTypeRule]


class RftoolsSpawnerItem(BaseModel):
    amount: float
    object: Union[Dict[str, str], List[ItemInput], List]


class BendingMachineRecipe(MIElectricRecipe):
    type: Literal["extended_industrialization:bending_machine"]


class SmeltingRecipe(Recipe):
    type: Literal["minecraft:smelting"]
    cookingtime: Optional[int] = None
    experience: Optional[float] = None
    ingredient: ItemInputUse
    result: ItemOutputUse


class ExtractorRecipe(Recipe):
    type: Literal["xycraft_machines:extractor"]
    adjacent: Any = None
    hidden: Optional[bool] = None
    output: ItemOutputUse
    target: Any = None
    ticks: int
    catalyst: Optional[Any] = None
    valid_directions: Optional[Any] = None
    waterlogged_fluid: Any = None


class BlastingRecipe(Recipe):
    type: Literal["minecraft:blasting"]
    cookingtime: int
    experience: float
    ingredient: ItemInputUse
    result: ItemOutputUse


class CrushingRecipe(Recipe):
    type: Literal["create:crushing"]
    ingredients: List[ItemInput]
    processing_time: Optional[int] = Field(default=None,
                                           validation_alias=AliasChoices(
                                               "processing_time",
                                               "processingTime"))
    results: List[ItemOutput]


class DeployingRecipe(Recipe):
    type: Literal["create:deploying"]
    ingredients: List[ItemInput]
    keep_held_item: Optional[bool] = None
    results: List[ItemOutput]


class CuttingMachineRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:cutting_machine"]


class SpawnerRecipe(Recipe):
    type: Literal["rftoolsutility:spawner"]
    entity: str
    id: str
    item1: RftoolsSpawnerItem
    item2: RftoolsSpawnerItem
    item3: RftoolsSpawnerItem
    power: int


class MaceratorRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:macerator"]


class CompressorRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:compressor"]


class SophisticatedCoreCondition(BaseModel):
    type: Literal["sophisticatedcore:item_enabled"]
    itemRegistryName: str


class CreateDragonsCondition(BaseModel):
    type: Literal["create_dragons_plus:config_feature"]
    feature: str


class CraftingSpecialMapExtendingRecipe(Recipe):
    type: Literal["minecraft:crafting_special_mapextending"]


class UpgradeNextTierRecipe(Recipe):
    type: Literal["sophisticatedcore:upgrade_next_tier"]
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ItemOutputUse


class ClientRecipeTrackerRecipe(Recipe):
    type: Literal["almostunified:client_recipe_tracker"]
    namespace: str
    recipes: List[str]


class BlastFurnaceRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:blast_furnace"]


class HeatExchangerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:heat_exchanger"]


class UnpackerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:unpacker"]


class AlchemistCauldronBrewRecipe(Recipe):
    type: Literal["irons_spellbooks:alchemist_cauldron_brew"]
    base_fluid: FluidInput
    input: ItemInputUse
    results: List[FluidOutput]
    byproduct: Optional[ItemOutputUse] = None


class MixingRecipe(Recipe):
    type: Literal["create:mixing"]
    ingredients: List[Union[ItemInput, FluidInput]]
    results: List[FluidOutput]
    heat_requirement: Optional[str] = None


class SandpaperPolishingRecipe(Recipe):
    type: Literal["create:sandpaper_polishing"]
    ingredients: List[ItemInput]
    results: List[ItemOutput]


class SizedUpgradeRecipe(Recipe):
    type: Literal["apotheosis:sized_upgrade_recipe"]
    addition: Union[ItemInput, Dict[str, Union[int, str]]]
    base: ItemInputUse
    result: ItemOutputUse
    template: ItemInputUse


class DoubleChestTierUpgradeRecipe(Recipe):
    type: Literal["sophisticatedstorage:double_chest_tier_upgrade"]
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ItemOutputUse


class BeheadingRecipe(Recipe):
    type: Literal["mob_grinding_utils:beheading"]
    entity: str
    result: ItemOutputUse


class ReforgingRecipe(Recipe):
    type: Literal["apotheosis:reforging"]
    level_cost: int
    material_cost: int
    rarity: str
    sigil_cost: int
    tables: Any


class AlloySmelterRecipe(MIElectricRecipe):
    type: Literal["extended_industrialization:alloy_smelter"]


class CraftingSpecialShapedOmniDirectionalRecipe(Recipe):
    type: Literal[
        "integrateddynamics:crafting_special_shaped_omni_directional"]
    group: str
    pattern: List[str]
    key: Dict[str, ItemInput]


class CentrifugeRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:centrifuge"]


class AlchemistCauldronEmptyRecipe(Recipe):
    type: Literal["irons_spellbooks:alchemist_cauldron_empty"]
    fluid: FluidInput
    input: ItemInputUse
    result: ItemOutputUse


class CopyComponentsRecipe(Recipe):
    type: Literal["mcjtylib:copy_components"]
    recipe: CraftingShapedRecipe


class ItemApplicationRecipe(Recipe):
    type: Literal["create:item_application"]
    ingredients: List[ItemInput]
    results: List[ItemOutput]


class SmithingTransformRecipe(Recipe):
    type: Literal["minecraft:smithing_transform"]
    template: ItemInputUse
    base: ItemInputUse
    addition: ItemInputUse
    result: Any


class ProbabilisticFluidOutput(BaseModel):
    fluid: str
    amount: int
    probability: float


class SyringePattern(BaseModel):
    key: Dict[str, ItemInput]
    pattern: List[str]


class TagOutput(BaseModel):
    tag: Union[Dict[str, Union[str, int]], str]
    chance: Optional[float] = None


class NeoforgeCompoundIngredientSimple(BaseModel):
    type: Literal["neoforge:compound"]
    ingredients: List[ItemInput]


class NeoforgeNotCondition(BaseModel):
    type: Literal["neoforge:not"]
    value: Dict[str, Union[str, Dict[str, str]]]


class ComponentMerger(BaseModel):
    active: Optional[str] = None
    pages: str
    title: Optional[str] = None


class StatChange(BaseModel):
    type: str
    value: Any


class CokeOvenRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:coke_oven"]


class SyringeRecipe(Recipe):
    type: Literal["rftoolsutility:syringe"]
    group: str
    mob: str
    pattern: SyringePattern
    result: ItemOutputUse
    syringe: int


class SmithingTrimRecipe(Recipe):
    type: Literal["minecraft:smithing_trim"]
    addition: ItemInputUse
    base: ItemInputUse
    template: ItemInputUse


class CopyComponentsRecipeSimple(Recipe):
    type: Literal["pipez:copy_components"]
    source: ItemInputUse
    target: ItemInputUse
    components: List[str]


class SmokingRecipe(Recipe):
    type: Literal["minecraft:smoking"]
    cookingtime: int
    experience: float
    ingredient: ItemInputUse
    result: ItemOutputUse


class AlchemistCauldronFillRecipe(Recipe):
    type: Literal["irons_spellbooks:alchemist_cauldron_fill"]
    fluid: FluidInput
    input: ItemInputUse
    result: ItemOutputUse


class CraftingSpecialBannerDuplicateRecipe(Recipe):
    type: Literal["minecraft:crafting_special_bannerduplicate"]


class MechanicalSqueezerRecipe(Recipe):
    type: Literal["integrateddynamics:mechanical_squeezer"]
    input_item: Optional[ItemInput] = None
    output_items: Optional[List[ItemOutput]] = None
    output_fluid: Optional[FluidOutput] = None
    duration: int


class FillingRecipe(Recipe):
    type: Literal["create:filling"]
    ingredients: Any
    results: List[ItemOutput]


class StorageTierUpgradeRecipe(Recipe):
    type: Literal["sophisticatedstorage:storage_tier_upgrade"]
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ItemOutputUse


class CraftingShapedSimpleRecipe(Recipe):
    type: Literal["crafting_shaped"]
    pattern: List[str]
    key: Dict[str, ItemInput]
    result: Dict[str, str]


class ElectrolyzerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:electrolyzer"]


class DryingBasinRecipe(Recipe):
    type: Literal["integrateddynamics:drying_basin"]
    input_item: Optional[ItemInput] = None
    input_fluid: Optional[FluidInput] = None
    duration: int
    output_item: Dict[str, str]


class EndingRecipe(Recipe):
    type: Literal["create_dragons_plus:ending"]
    ingredients: List[ItemInput]
    results: List[ItemOutput]


class SplashingRecipe(Recipe):
    type: Literal["create:splashing"]
    ingredients: List[ItemInput]
    results: List[ItemOutput]


class StorageDyeRecipe(Recipe):
    type: Literal["sophisticatedstorage:storage_dye"]


class CraftingSpecialNbtClearRecipe(Recipe):
    type: Literal["integrateddynamics:crafting_special_nbt_clear"]
    item: ItemInputUse


class SqueezerRecipe(Recipe):
    type: Literal["integrateddynamics:squeezer"]
    input_item: ItemInputUse
    output_items: Optional[List[ItemOutput]] = None
    output_fluid: Optional[FluidOutput] = None
    duration: Optional[int] = None


class ImplosionCompressorRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:implosion_compressor"]


class CampfireCookingRecipe(Recipe):
    type: Literal["minecraft:campfire_cooking"]
    cookingtime: int
    experience: float
    ingredient: ItemInputUse
    show_notification: Optional[bool] = False


class CraftingSpecialVariableCopyRecipe(Recipe):
    type: Literal["integrateddynamics:crafting_special_variable_copy"]


class PrintingTableMergingRecipe(Recipe):
    type: Literal["bibliocraft:printing_table_merging"]
    component_mergers: Dict[str, ComponentMerger]
    duration: int
    ingredient: ItemInputUse
    result: ItemOutputUse


class SpawnerModifierRecipe(Recipe):
    type: Literal["apothic_spawners:spawner_modifier"]
    mainhand: ItemInputUse
    offhand: Optional[ItemInput] = None
    consumes_offhand: Optional[bool] = None
    stat_changes: List[StatChange]


class FluidTagIngredient(BaseModel):
    type: Literal["fluid_tag"]
    amount: int
    fluid_tag: str


class SequenceStep(BaseModel):
    type: str
    ingredients: List[Union[ItemInput, FluidInput]]
    results: List[Union[ItemInput, Dict[str, str]]]


class FluidStackWithComponents(BaseModel):
    type: Literal["fluid_stack"]
    amount: int
    fluid: str
    components: Optional[Dict[str, Any]] = None


class ResultWithComponents(BaseModel):
    count: int
    id: str
    components: Optional[Dict[str, Any]] = None


class SolidifierResult(BaseModel):
    Name: str


class SolidifierRule(BaseModel):
    block: str
    predicate_type: str


class GemInput(BaseModel):
    type: Literal["apotheosis:gem"]
    purity: str


class SalvageOutput(BaseModel):
    max_count: int
    min_count: int
    stack: Dict[str, Union[str, int]]


class Requirements(BaseModel):
    eterna: int
    quanta: int
    arcana: int


class ComponentsItemInput(BaseModel):
    type: Literal["neoforge:components"]
    amount: int
    components: Dict[str, Any]
    items: str


class MixedIngredient(BaseModel):
    count: Optional[int] = None
    item: Optional[str] = None
    amount: Optional[int] = None
    fluid: Optional[str] = None
    tag: Optional[str] = None


class PressingRecipe(Recipe):
    type: Literal["create:pressing"]
    ingredients: Any
    results: List[ItemOutput]


class SolidifyRecipe(Recipe):
    type: Literal["mob_grinding_utils:solidify"]
    fluidAmount: int
    ingredient: ItemInputUse
    result: ItemOutputUse


class CompactingRecipe(Recipe):
    type: Literal["create:compacting"]
    ingredients: List[Dict]
    results: List[ItemOutput]
    heat_requirement: Optional[str] = None


class MechanicalDryingBasinRecipe(Recipe):
    type: Literal["integrateddynamics:mechanical_drying_basin"]
    input_item: Optional[ItemInput] = None
    input_fluid: Optional[FluidInput] = None
    duration: int
    output_item: Dict[str, str]


class MechanicalCraftingRecipe(Recipe):
    type: Literal["create:mechanical_crafting"]
    accept_mirrored: bool
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ItemOutputUse
    show_notification: Optional[bool] = None


class SequencedAssemblyRecipe(Recipe):
    type: Literal["create:sequenced_assembly"]
    ingredient: ItemInputUse
    loops: int
    results: List[Union[ItemInput, Dict[str, str]]]
    sequence: List[Dict[str, Any]]
    transitional_item: ItemInputUse


class VacuumFreezerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:vacuum_freezer"]


class ShulkerBoxFromVanillaShapelessRecipe(Recipe):
    type: Literal["sophisticatedstorage:shulker_box_from_vanilla_shapeless"]
    ingredients: List[ItemInput]
    result: ResultWithComponents


class PyrolyseOvenRecipe(MIElectricRecipe):
    type: Literal["industrialization_overdrive:pyrolyse_oven"]


class SolidifierRecipe(Recipe):
    type: Literal["xycraft_machines:solidifier"]
    result: SolidifierResult
    rule: SolidifierRule


class BlenderRecipe(Recipe):
    type: Literal["xycraft_machines:blender"]
    ingredient_a: FluidInput
    ingredient_b: Optional[FluidInput] = None
    output: ItemOutputUse
    ticks: int


class DoubleChestTierUpgradeShapelessRecipe(Recipe):
    type: Literal["sophisticatedstorage:double_chest_tier_upgrade_shapeless"]
    ingredients: List[ItemInput]
    result: ItemOutputUse


class CraftingDecoratedPotRecipe(Recipe):
    type: Literal["minecraft:crafting_decorated_pot"]


class CraftingSpecialShapedOmniDirectional3Recipe(Recipe):
    type: Literal[
        "integrateddynamics:crafting_special_shaped_omni_directional_3"]
    group: str
    pattern: List[str]
    key: Dict[str, ItemInput]


class PressurizerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:pressurizer"]


class SalvagingRecipe(Recipe):
    type: Literal["apotheosis:salvaging"]
    input: Any
    outputs: List[SalvageOutput]


class SmithingBackpackUpgradeRecipe(Recipe):
    type: Literal["sophisticatedbackpacks:smithing_backpack_upgrade"]
    addition: ItemInputUse
    base: ItemInputUse
    result: ItemOutputUse
    template: ItemInputUse


class HauntingRecipe(Recipe):
    type: Literal["create:haunting"]
    ingredients: List[ItemInput]
    results: List[ItemOutput]


class DistillationTowerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:distillation_tower"]


class DistilleryRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:distillery"]


class PrintingTableBindingTypewriterPagesRecipe(Recipe):
    type: Literal["bibliocraft:printing_table_binding_typewriter_pages"]
    duration: int
    ingredient: ItemInputUse


class StorageBlockUpgradeRecipe(Recipe):
    type: Literal["refinedstorage:storage_block_upgrade"]
    to: str


class BigBookCloningRecipe(Recipe):
    type: Literal["bibliocraft:big_book_cloning"]


class FusionReactorRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:fusion_reactor"]


class BuildingsRecipe(Recipe):
    type: Literal["xycraft_machines:buildings"]
    ingredient: List[MixedIngredient]


class PrintingTableCloningRecipe(Recipe):
    type: Literal["bibliocraft:printing_table_cloning"]
    data_components: List[str]
    duration: int
    ingredients: List[ItemInput]
    result: ItemOutputUse


class InfusionRecipe(Recipe):
    type: Literal["apothic_enchanting:infusion"]
    input: ItemInputUse
    requirements: Requirements
    max_requirements: Optional[Requirements] = None
    result: ItemOutputUse


class CanningMachineRecipe(MIElectricRecipe):
    type: Literal["extended_industrialization:canning_machine"]


class CraftingSpecialEnergycontainerCombinationRecipe(Recipe):
    type: Literal[
        "integrateddynamics:crafting_special_energycontainer_combination"]
    item: ItemInputUse
    maxCapacity: int


class SocketingRecipe(Recipe):
    type: Literal["apotheosis:socketing"]


class OilDrillingRigRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:oil_drilling_rig"]


class UncraftingRecipe(Recipe):
    type: Literal["twilightforest:uncrafting"]
    cost: int
    input: ItemInputUse
    key: Dict[str, ItemInput]
    pattern: List[str]
    input_count: Optional[int] = None


class WiremillRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:wiremill"]


class MaliceRecipe(Recipe):
    type: Literal["apotheosis:malice"]


class ArkMelterRecipe(Recipe):
    type: Literal["xycraft_machines:ark_melter"]
    entropy: int
    ingredient: Dict[str, Union[int, str]]
    output: Dict[str, Union[int, str]]
    ticks: int


class CraftingSpecialFireworkRocketRecipe(Recipe):
    type: Literal["minecraft:crafting_special_firework_rocket"]


class FluidStorageDiskUpgradeRecipe(Recipe):
    type: Literal["refinedstorage:fluid_storage_disk_upgrade"]
    to: str


class SupremacyRecipe(Recipe):
    type: Literal["apotheosis:supremacy"]


class FlatTopBarrelToggleRecipe(Recipe):
    type: Literal["sophisticatedstorage:flat_top_barrel_toggle"]


class RefineryRecipe(Recipe):
    type: Literal["xycraft_machines:refinery"]
    ingredient_a: FluidInput
    ingredient_b: Optional[Dict[str, Union[int, str]]] = None
    output_a: ItemOutputUse
    output_b: Optional[ItemOutput] = None
    ticks: int


class BackpackUpgradeRecipe(Recipe):
    type: Literal["sophisticatedbackpacks:backpack_upgrade"]
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ItemOutputUse


class BarrelMaterialRecipe(Recipe):
    type: Literal["sophisticatedstorage:barrel_material"]


class ModuleResetRecipe(Recipe):
    type: Literal["modularrouters:module_reset"]


class ToolboxDyeingRecipe(Recipe):
    type: Literal["create:toolbox_dyeing"]


class CraftingSpecialBookCloningRecipe(Recipe):
    type: Literal["minecraft:crafting_special_bookcloning"]


class CraftingSpecialShapelessOmniDirectionalRecipe(Recipe):
    type: Literal[
        "integrateddynamics:crafting_special_shapeless_omni_directional"]
    group: str
    ingredients: List[ItemInput]


class PurityUpgradeIngredient(BaseModel):
    count: int
    item: str


class CrusherResult(BaseModel):
    chance: int
    result: ItemOutputUse


class MobGrindingFluidInput(BaseModel):
    type: Literal["mob_grinding_utils:fluid"]
    advanced: bool
    value: Dict[str, Union[int, str]]


class EmptyingResult(BaseModel):
    id: str
    amount: Optional[int] = None


class ComponentsIngredientList(BaseModel):
    type: Literal["neoforge:components"]
    components: Dict[str, Any]
    items: str


class ExperienceCost(BaseModel):
    type: str
    data_component: str
    normal_cost: float
    treasure_cost: float


class ShulkerBoxFromChestRecipe(Recipe):
    type: Literal["sophisticatedstorage:shulker_box_from_chest"]
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ItemOutputUse


class PurityUpgradeRecipe(Recipe):
    type: Literal["apotheosis:purity_upgrade"]
    left: List[PurityUpgradeIngredient]
    purity: str
    right: List[PurityUpgradeIngredient]


class CentrifugeRecipeSimple(Recipe):
    type: Literal["xycraft_machines:centrifuge"]
    input: FluidInput
    output_a: FluidOutput
    ticks: int


class BreakerModuleRecipe(Recipe):
    type: Literal["modularrouters:breaker_module"]


class GrindingRecipe(Recipe):
    type: Literal["create_enchantment_industry:grinding"]
    ingredients: List[ItemInput]
    results: List[FluidOutput]


class CraftingSpecialShieldDecorationRecipe(Recipe):
    type: Literal["minecraft:crafting_special_shielddecoration"]


class PolarizerRecipe(MIElectricRecipe):
    type: Literal["modern_industrialization:polarizer"]


class CasketRepairRecipe(Recipe):
    type: Literal["twilightforest:casket_repair_recipe"]


class FreezingRecipe(Recipe):
    type: Literal["create_dragons_plus:freezing"]
    ingredients: List[ItemInput]
    results: List[ItemOutput]


class GenericWoodStorageRecipe(Recipe):
    type: Literal["sophisticatedstorage:generic_wood_storage"]
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ResultWithComponents


class ComposterRecipe(MIElectricRecipe):
    type: Literal["extended_industrialization:composter"]


class CraftingSpecialMapCloningRecipe(Recipe):
    type: Literal["minecraft:crafting_special_mapcloning"]


class FluidStorageBlockUpgradeRecipe(Recipe):
    type: Literal["refinedstorage:fluid_storage_block_upgrade"]
    to: str


class UpgradeWithEnchantedBookRecipe(Recipe):
    type: Literal["refinedstorage:upgrade_with_enchanted_book"]
    enchantment: str
    level: Optional[int] = None


class KeepNbtInfusionRecipe(Recipe):
    type: Literal["apothic_enchanting:keep_nbt_infusion"]
    input: ItemInputUse
    requirements: Requirements
    max_requirements: Requirements
    result: ItemOutputUse


class CrusherRecipe(Recipe):
    type: Literal["xycraft_machines:crusher"]
    ingredient: Dict[str, Union[int, str]]
    results: List[CrusherResult]
    ticks: int


class EmperorsClothRecipe(Recipe):
    type: Literal["twilightforest:emperors_cloth_recipe"]


class OreTapRecipe(Recipe):
    type: Literal["xycraft_machines:ore_tap"]
    node_rule: SolidifierRule
    output: FluidOutput
    ticks: int


class CraftingSpecialRainbowableDyeRecipe(Recipe):
    type: Literal[
        "extended_industrialization:crafting_special_rainbowable_dye"]


class FluidTankDrainRecipe(Recipe):
    type: Literal["xycraft_machines:fluid_tank_drain"]
    container: ItemOutputUse
    fluid_result: FluidOutput
    item_result: ItemOutputUse


class ChickenFeedRecipe(Recipe):
    type: Literal["mob_grinding_utils:chicken_feed"]
    ingredients: List[Union[ItemInput, MobGrindingFluidInput]]
    result: ItemOutputUse


class EmptyingRecipeSimple(Recipe):
    type: Literal["create:emptying"]
    ingredients: List[ItemInput]
    results: List[EmptyingResult]


class ScepterRepairRecipe(Recipe):
    type: Literal["twilightforest:scepter_repair"]
    durability: int
    repair_ingredients: List[Union[List[ComponentsIngredientList], ItemInput]]
    scepter: str


class CraftingSpecialShulkerBoxColoringRecipe(Recipe):
    type: Literal["minecraft:crafting_special_shulkerboxcoloring"]


class PotionCharmCraftingRecipe(Recipe):
    type: Literal["apotheosis:potion_charm_crafting"]
    key: Dict[str, ItemInput]
    pattern: List[str]


class StorageDiskUpgradeRecipe(Recipe):
    type: Literal["refinedstorage:storage_disk_upgrade"]
    to: str


class IsolatorRecipe(Recipe):
    type: Literal["xycraft_machines:isolator"]
    output: int
    target: SolidifierRule


class PrintingTableCloningWithEnchantmentsRecipe(Recipe):
    type: Literal["bibliocraft:printing_table_cloning_with_enchantments"]
    duration: int
    experience_cost: ExperienceCost
    ingredients: List[ItemInput]
    result: ItemOutputUse


class EssenceRepairRecipe(Recipe):
    type: Literal["twilightforest:essence_repair_recipe"]


class CopyBlueprintRecipe(Recipe):
    type: Literal["mi_tweaks:copy_blueprint"]
    ingredients: List[ItemInput]
    result: Dict[str, Union[str, int]]


class GuideBookRecipe(Recipe):
    type: Literal["modularrouters:guide_book"]


class MazeMapCloningRecipe(Recipe):
    type: Literal["twilightforest:maze_map_cloning_recipe"]


class ClearComponentsRecipe(Recipe):
    type: Literal["pipez:clear_components"]
    item: ItemInputUse
    components: List[str]


class CraftingSpecialFireworkStarFadeRecipe(Recipe):
    type: Literal["minecraft:crafting_special_firework_star_fade"]


class CraftingSpecialTippedArrowRecipe(Recipe):
    type: Literal["minecraft:crafting_special_tippedarrow"]


class CraftingSpecialArmorDyeRecipe(Recipe):
    type: Literal["minecraft:crafting_special_armordye"]


class StorageTierUpgradeShapelessRecipe(Recipe):
    type: Literal["sophisticatedstorage:storage_tier_upgrade_shapeless"]
    ingredients: List[ItemInput]
    result: ItemOutputUse


class ExtruderModule1Recipe(Recipe):
    type: Literal["modularrouters:extruder_module_1"]


class CraftingSpecialRepairItemRecipe(Recipe):
    type: Literal["minecraft:crafting_special_repairitem"]


class SquasherRecipe(Recipe):
    type: Literal["xycraft_machines:squasher"]
    ingredient: Dict[str, Union[int, str]]
    result: ItemOutputUse
    ticks: int


class CraftingSpecialSuspiciousStewRecipe(Recipe):
    type: Literal["minecraft:crafting_special_suspiciousstew"]


class AtmosphericVacuumRecipe(Recipe):
    type: Literal["xycraft_machines:atmospheric_vacuum"]
    output: FluidOutput
    ticks: int
    valid_biomes: str


class UpgradeClearRecipe(Recipe):
    type: Literal["sophisticatedcore:upgrade_clear"]


class WithdrawalRecipe(Recipe):
    type: Literal["apotheosis:withdrawal"]


class UnnamingRecipe(Recipe):
    type: Literal["apotheosis:unnaming"]


class NoTemplateSmithingRecipe(Recipe):
    type: Literal["twilightforest:no_template_smithing"]
    addition: ItemInputUse
    additional_data: Dict[str, Dict]
    base: ItemInputUse


class MoonwormQueenRepairRecipe(Recipe):
    type: Literal["twilightforest:moonworm_queen_repair_recipe"]


class BackpackDyeRecipe(Recipe):
    type: Literal["sophisticatedbackpacks:backpack_dye"]


class FluidTankFillRecipe(Recipe):
    type: Literal["xycraft_machines:fluid_tank_fill"]
    fluid: Dict[str, Union[int, str]]
    item: ItemOutputUse
    result: ItemOutputUse


class CraftingSpecialFacadeRecipe(Recipe):
    type: Literal["integrateddynamics:crafting_special_facade"]


class PotionCharmInfusionRecipe(Recipe):
    type: Literal["apotheosis:potion_charm_infusion"]
    max_requirements: Dict[str, float]
    requirements: Dict[str, float]


class CryChamberRecipe(Recipe):
    type: Literal["xycraft_machines:cry_chamber"]
    entropy: int
    ingredient: Dict[str, Union[int, str]]
    output: ItemOutputUse
    ticks: int


class MagicMapCloningRecipe(Recipe):
    type: Literal["twilightforest:magic_map_cloning_recipe"]


class CraftingSpecialFireworkStarRecipe(Recipe):
    type: Literal["minecraft:crafting_special_firework_star"]


class BasicBackpackRecipe(Recipe):
    type: Literal["sophisticatedbackpacks:basic_backpack"]
    key: Dict[str, ItemInput]
    pattern: List[str]
    result: ItemOutputUse


class AddSocketsRecipe(Recipe):
    type: Literal["apotheosis:add_sockets"]
    input: ItemInputUse
    max_sockets: int


class ItemCopyingRecipe(Recipe):
    type: Literal["create:item_copying"]


class TypewriterPageCloningRecipe(Recipe):
    type: Literal["bibliocraft:typewriter_page_cloning"]


class FallbackUnknown(Recipe):
    type: Optional[str]


RecipeUnion = Annotated[
    Union[
        ChemicalReactorRecipe,
        CraftingShapedRecipe,
        PackerRecipe,
        MixerRecipe,
        QuarryRecipe,
        CuttingRecipe,
        ForgeHammerRecipe,
        MillingRecipe,
        AssemblerRecipe,
        StonecuttingRecipe,
        CraftingShapelessRecipe,
        BendingMachineRecipe,
        SmeltingRecipe,
        ExtractorRecipe,
        BlastingRecipe,
        CrushingRecipe,
        DeployingRecipe,
        CuttingMachineRecipe,
        SpawnerRecipe,
        MaceratorRecipe,
        CompressorRecipe,
        CraftingSpecialMapExtendingRecipe,
        UpgradeNextTierRecipe,
        ClientRecipeTrackerRecipe,
        BlastFurnaceRecipe,
        HeatExchangerRecipe,
        UnpackerRecipe,
        AlchemistCauldronBrewRecipe,
        MixingRecipe,
        SandpaperPolishingRecipe,
        SizedUpgradeRecipe,
        DoubleChestTierUpgradeRecipe,
        BeheadingRecipe,
        ReforgingRecipe,
        AlloySmelterRecipe,
        CraftingSpecialShapedOmniDirectionalRecipe,
        CentrifugeRecipe,
        AlchemistCauldronEmptyRecipe,
        CopyComponentsRecipe,
        ItemApplicationRecipe,
        SmithingTransformRecipe,
        CokeOvenRecipe,
        SyringeRecipe,
        SmithingTrimRecipe,
        CopyComponentsRecipeSimple,
        SmokingRecipe,
        AlchemistCauldronFillRecipe,
        CraftingSpecialBannerDuplicateRecipe,
        MechanicalSqueezerRecipe,
        FillingRecipe,
        StorageTierUpgradeRecipe,
        CraftingShapedSimpleRecipe,
        ElectrolyzerRecipe,
        DryingBasinRecipe,
        EndingRecipe,
        SplashingRecipe,
        StorageDyeRecipe,
        CraftingSpecialNbtClearRecipe,
        SqueezerRecipe,
        ImplosionCompressorRecipe,
        CampfireCookingRecipe,
        CraftingSpecialVariableCopyRecipe,
        PrintingTableMergingRecipe,
        SpawnerModifierRecipe,
        PressingRecipe,
        SolidifyRecipe,
        CompactingRecipe,
        MechanicalDryingBasinRecipe,
        MechanicalCraftingRecipe,
        SequencedAssemblyRecipe,
        VacuumFreezerRecipe,
        ShulkerBoxFromVanillaShapelessRecipe,
        PyrolyseOvenRecipe,
        SolidifierRecipe,
        BlenderRecipe,
        DoubleChestTierUpgradeShapelessRecipe,
        CraftingDecoratedPotRecipe,
        CraftingSpecialShapedOmniDirectional3Recipe,
        PressurizerRecipe,
        SalvagingRecipe,
        SmithingBackpackUpgradeRecipe,
        HauntingRecipe,
        DistillationTowerRecipe,
        DistilleryRecipe,
        PrintingTableBindingTypewriterPagesRecipe,
        StorageBlockUpgradeRecipe,
        BigBookCloningRecipe,
        FusionReactorRecipe,
        BuildingsRecipe,
        PrintingTableCloningRecipe,
        InfusionRecipe,
        CanningMachineRecipe,
        CraftingSpecialEnergycontainerCombinationRecipe,
        SocketingRecipe,
        OilDrillingRigRecipe,
        UncraftingRecipe,
        WiremillRecipe,
        MaliceRecipe,
        ArkMelterRecipe,
        CraftingSpecialFireworkRocketRecipe,
        FluidStorageDiskUpgradeRecipe,
        SupremacyRecipe,
        FlatTopBarrelToggleRecipe,
        RefineryRecipe,
        BackpackUpgradeRecipe,
        BarrelMaterialRecipe,
        ModuleResetRecipe,
        ToolboxDyeingRecipe,
        CraftingSpecialBookCloningRecipe,
        CraftingSpecialShapelessOmniDirectionalRecipe,
        ShulkerBoxFromChestRecipe,
        PurityUpgradeRecipe,
        CentrifugeRecipeSimple,
        BreakerModuleRecipe,
        GrindingRecipe,
        CraftingSpecialShieldDecorationRecipe,
        PolarizerRecipe,
        CasketRepairRecipe,
        FreezingRecipe,
        GenericWoodStorageRecipe,
        ComposterRecipe,
        CraftingSpecialMapCloningRecipe,
        FluidStorageBlockUpgradeRecipe,
        UpgradeWithEnchantedBookRecipe,
        KeepNbtInfusionRecipe,
        CrusherRecipe,
        EmperorsClothRecipe,
        OreTapRecipe,
        CraftingSpecialRainbowableDyeRecipe,
        FluidTankDrainRecipe,
        ChickenFeedRecipe,
        EmptyingRecipeSimple,
        ScepterRepairRecipe,
        CraftingSpecialShulkerBoxColoringRecipe,
        PotionCharmCraftingRecipe,
        StorageDiskUpgradeRecipe,
        IsolatorRecipe,
        PrintingTableCloningWithEnchantmentsRecipe,
        EssenceRepairRecipe,
        CopyBlueprintRecipe,
        GuideBookRecipe,
        MazeMapCloningRecipe,
        ClearComponentsRecipe,
        CraftingSpecialFireworkStarFadeRecipe,
        CraftingSpecialTippedArrowRecipe,
        CraftingSpecialArmorDyeRecipe,
        StorageTierUpgradeShapelessRecipe,
        ExtruderModule1Recipe,
        CraftingSpecialRepairItemRecipe,
        SquasherRecipe,
        CraftingSpecialSuspiciousStewRecipe,
        AtmosphericVacuumRecipe,
        UpgradeClearRecipe,
        WithdrawalRecipe,
        UnnamingRecipe,
        NoTemplateSmithingRecipe,
        MoonwormQueenRepairRecipe,
        BackpackDyeRecipe,
        FluidTankFillRecipe,
        CraftingSpecialFacadeRecipe,
        PotionCharmInfusionRecipe,
        CryChamberRecipe,
        MagicMapCloningRecipe,
        CraftingSpecialFireworkStarRecipe,
        BasicBackpackRecipe,
        AddSocketsRecipe,
        ItemCopyingRecipe,
        TypewriterPageCloningRecipe,
    ],
    Discriminator("type"),
]


class RecipeCollection(BaseModel):
    recipes: List[RecipeUnion | FallbackUnknown]

    class Config:
        extra = "forbid"


def parse_recipe_collection(recipe_data: Dict[str, Any]) -> RecipeCollection:
    return RecipeCollection(**recipe_data)


import pydantic

if __name__ == "__main__":
    content = json.loads(
        Path(
            "/home/haxscramper/.local/share/multimc/instances/1.21.1 V2/.minecraft/kubejs/server_scripts/all_recipes.json"
        ).read_text())

    recipe_types = get_args(RecipeUnion.__args__[0])
    type_values = set()

    for recipe_type in recipe_types:
        literal_values = get_args(recipe_type.__annotations__['type'])
        type_values.add(literal_values[0])

    examples = {}
    for item in content:
        if item["type"] not in examples and item["type"] not in type_values:
            examples[item["type"]] = item

    Path("/tmp/mi_examples.json").write_text(
        json.dumps([v for k, v in examples.items()], indent=2))

    collection = dict(recipes=content)

    p = Path("/tmp/mi_validation_error.txt")
    try:
        model = parse_recipe_collection(collection)
        p.write_text("no validation errors")

    except ValidationError as err:
        p.write_text(str(err))
        with p.with_name("validation_dump.txt").open("w") as file:
            for it in err.errors():
                file.write(str(it))
                file.write("\n")
