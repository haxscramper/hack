#!/usr/bin/env python

from beartype.typing import Optional, List, Tuple, Dict
from beartype import beartype
from pydantic import BaseModel


class ProgressBarParameters(BaseModel):
    x: int
    y: int
    texture: str


class RecipeEfficiencyBarParameters(BaseModel):
    x: int
    y: int


class EnergyBarParameters(BaseModel):
    x: int
    y: int


class SlotPosition(BaseModel):
    x: int
    y: int
    cols: int
    rows: int


class ItemSlots(BaseModel):
    positions: List[SlotPosition]


class FluidSlots(BaseModel):
    positions: List[SlotPosition]


class GuiParameters(BaseModel):
    background_height: Optional[int] = None


class MachineData(BaseModel):
    english_name: str
    machine: str
    recipe_type: str
    item_input_count: int
    item_output_count: int
    fluid_input_count: int
    fluid_output_count: int
    gui_params: GuiParameters
    progress_bar: ProgressBarParameters
    efficiency_bar: RecipeEfficiencyBarParameters
    energy_bar: EnergyBarParameters
    item_slots: ItemSlots
    fluid_slots: FluidSlots
    front_overlay: bool
    top_overlay: bool
    side_overlay: bool
    tiers: int
    io_bucket_capacity: int
    size: Optional[Tuple[int, int]] = (200, 100)


mi_fixed_machines_list = [
    MachineData(
        english_name="Distillation Tower",
        machine="distillation_tower",
        recipe_type="DISTILLATION TOWER",
        item_input_count=9,
        item_output_count=3,
        fluid_input_count=2,
        fluid_output_count=0,
        gui_params=GuiParameters(background_height=186),
        progress_bar=ProgressBarParameters(x=70, y=40, texture="circuit"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=48, y=86),
        energy_bar=EnergyBarParameters(x=14, y=44),
        item_slots=ItemSlots(positions=[]),
        fluid_slots=FluidSlots(positions=[
            SlotPosition(x=30, y=35, cols=1, rows=1),
            SlotPosition(x=98, y=27, cols=5, rows=2),
        ]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=False,
        tiers=16,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Assembler",
        machine="assembler",
        recipe_type="ASSEMBLER",
        item_input_count=9,
        item_output_count=3,
        fluid_input_count=2,
        fluid_output_count=0,
        gui_params=GuiParameters(background_height=186),
        progress_bar=ProgressBarParameters(x=105, y=45, texture="circuit"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=48, y=86),
        energy_bar=EnergyBarParameters(x=14, y=44),
        item_slots=ItemSlots(positions=[]),
        fluid_slots=FluidSlots(positions=[
            SlotPosition(x=30, y=47, cols=1, rows=1),
            SlotPosition(x=116, y=47, cols=3, rows=3)
        ]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=False,
        tiers=16,
        io_bucket_capacity=16,
        size=(300, 150),
    ),
    MachineData(
        english_name="Centrifuge",
        machine="centrifuge",
        recipe_type="CENTRIFUGE",
        item_input_count=1,
        item_output_count=4,
        fluid_input_count=1,
        fluid_output_count=4,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=65, y=33, texture="centrifuge"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=50, y=66),
        energy_bar=EnergyBarParameters(x=10, y=10),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=42, y=27, cols=1, rows=1),
            SlotPosition(x=93, y=27, cols=2, rows=2)
        ]),
        fluid_slots=FluidSlots(positions=[
            SlotPosition(x=42, y=45, cols=1, rows=1),
            SlotPosition(x=131, y=27, cols=2, rows=2)
        ]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=True,
        tiers=16,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Chemical Reactor",
        machine="chemical_reactor",
        recipe_type="CHEMICAL_REACTOR",
        item_input_count=3,
        item_output_count=3,
        fluid_input_count=3,
        fluid_output_count=3,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=88, y=35, texture="triple_arrow"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=50, y=66),
        energy_bar=EnergyBarParameters(x=12, y=35),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=30, y=27, cols=3, rows=1),
            SlotPosition(x=116, y=27, cols=3, rows=1)
        ]),
        fluid_slots=FluidSlots(positions=[
            SlotPosition(x=30, y=47, cols=3, rows=1),
            SlotPosition(x=116, y=47, cols=3, rows=1)
        ]),
        front_overlay=True,
        top_overlay=False,
        side_overlay=False,
        tiers=16,
        io_bucket_capacity=24,
        size=(200, 100),
    ),
    MachineData(
        english_name="Compressor",
        machine="compressor",
        recipe_type="COMPRESSOR",
        item_input_count=1,
        item_output_count=1,
        fluid_input_count=0,
        fluid_output_count=0,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=77, y=34, texture="compress"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=62),
        energy_bar=EnergyBarParameters(x=18, y=30),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=56, y=35, cols=1, rows=1),
            SlotPosition(x=102, y=35, cols=1, rows=1)
        ]),
        fluid_slots=FluidSlots(positions=[]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=True,
        tiers=23,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Cutting Machine",
        machine="cutting_machine",
        recipe_type="CUTTING_MACHINE",
        item_input_count=1,
        item_output_count=1,
        fluid_input_count=1,
        fluid_output_count=0,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=88, y=31, texture="slice"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=62),
        energy_bar=EnergyBarParameters(x=15, y=34),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=60, y=35, cols=1, rows=1),
            SlotPosition(x=120, y=35, cols=1, rows=1)
        ]),
        fluid_slots=FluidSlots(
            positions=[SlotPosition(x=40, y=35, cols=1, rows=1)]),
        front_overlay=True,
        top_overlay=False,
        side_overlay=False,
        tiers=23,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Distillery",
        machine="distillery",
        recipe_type="DISTILLERY",
        item_input_count=0,
        item_output_count=0,
        fluid_input_count=1,
        fluid_output_count=1,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=77, y=33, texture="arrow"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=62),
        energy_bar=EnergyBarParameters(x=18, y=30),
        item_slots=ItemSlots(positions=[]),
        fluid_slots=FluidSlots(positions=[
            SlotPosition(x=56, y=35, cols=1, rows=1),
            SlotPosition(x=102, y=35, cols=1, rows=1)
        ]),
        front_overlay=True,
        top_overlay=False,
        side_overlay=False,
        tiers=16,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Electrolyzer",
        machine="electrolyzer",
        recipe_type="ELECTROLYZER",
        item_input_count=1,
        item_output_count=4,
        fluid_input_count=1,
        fluid_output_count=4,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=66, y=35, texture="arrow"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=50, y=66),
        energy_bar=EnergyBarParameters(x=10, y=10),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=42, y=27, cols=1, rows=1),
            SlotPosition(x=93, y=27, cols=2, rows=2)
        ]),
        fluid_slots=FluidSlots(positions=[
            SlotPosition(x=42, y=47, cols=1, rows=1),
            SlotPosition(x=131, y=27, cols=2, rows=2)
        ]),
        front_overlay=True,
        top_overlay=False,
        side_overlay=True,
        tiers=16,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Furnace",
        machine="furnace",
        recipe_type="FURNACE",
        item_input_count=1,
        item_output_count=1,
        fluid_input_count=0,
        fluid_output_count=0,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=77, y=33, texture="arrow"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=62),
        energy_bar=EnergyBarParameters(x=18, y=30),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=56, y=35, cols=1, rows=1),
            SlotPosition(x=102, y=35, cols=1, rows=1)
        ]),
        fluid_slots=FluidSlots(positions=[]),
        front_overlay=True,
        top_overlay=False,
        side_overlay=False,
        tiers=23,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Macerator",
        machine="macerator",
        recipe_type="MACERATOR",
        item_input_count=1,
        item_output_count=4,
        fluid_input_count=0,
        fluid_output_count=0,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=77, y=33, texture="macerate"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=66),
        energy_bar=EnergyBarParameters(x=10, y=10),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=56, y=35, cols=1, rows=1),
            SlotPosition(x=102, y=27, cols=2, rows=2)
        ]),
        fluid_slots=FluidSlots(positions=[]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=False,
        tiers=23,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Mixer",
        machine="mixer",
        recipe_type="MIXER",
        item_input_count=4,
        item_output_count=2,
        fluid_input_count=2,
        fluid_output_count=2,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=103, y=33, texture="arrow"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=50, y=66),
        energy_bar=EnergyBarParameters(x=15, y=34),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=62, y=27, cols=2, rows=2),
            SlotPosition(x=129, y=27, cols=1, rows=2)
        ]),
        fluid_slots=FluidSlots(positions=[
            SlotPosition(x=42, y=27, cols=1, rows=2),
            SlotPosition(x=149, y=27, cols=1, rows=2)
        ]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=True,
        tiers=23,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Packer",
        machine="packer",
        recipe_type="PACKER",
        item_input_count=3,
        item_output_count=1,
        fluid_input_count=0,
        fluid_output_count=0,
        gui_params=GuiParameters(background_height=178),
        progress_bar=ProgressBarParameters(x=77, y=33, texture="arrow"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=74),
        energy_bar=EnergyBarParameters(x=18, y=30),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=56, y=18, cols=1, rows=3),
            SlotPosition(x=102, y=36, cols=1, rows=1)
        ]),
        fluid_slots=FluidSlots(positions=[]),
        front_overlay=True,
        top_overlay=False,
        side_overlay=False,
        tiers=20,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Polarizer",
        machine="polarizer",
        recipe_type="POLARIZER",
        item_input_count=2,
        item_output_count=1,
        fluid_input_count=0,
        fluid_output_count=0,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=77, y=30, texture="magnet"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=62),
        energy_bar=EnergyBarParameters(x=18, y=30),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=56, y=23, cols=1, rows=2),
            SlotPosition(x=102, y=32, cols=1, rows=1)
        ]),
        fluid_slots=FluidSlots(positions=[]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=False,
        tiers=16,
        io_bucket_capacity=16,
    ),
    MachineData(
        english_name="Wiremill",
        machine="wiremill",
        recipe_type="WIREMILL",
        item_input_count=1,
        item_output_count=1,
        fluid_input_count=0,
        fluid_output_count=0,
        gui_params=GuiParameters(),
        progress_bar=ProgressBarParameters(x=77, y=34, texture="wiremill"),
        efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=62),
        energy_bar=EnergyBarParameters(x=18, y=30),
        item_slots=ItemSlots(positions=[
            SlotPosition(x=56, y=35, cols=1, rows=1),
            SlotPosition(x=102, y=35, cols=1, rows=1)
        ]),
        fluid_slots=FluidSlots(positions=[]),
        front_overlay=True,
        top_overlay=True,
        side_overlay=False,
        tiers=20,
        io_bucket_capacity=16,
    ),
    MachineData(english_name="Unpacker",
                machine="unpacker",
                recipe_type="UNPACKER",
                item_input_count=1,
                item_output_count=2,
                fluid_input_count=0,
                fluid_output_count=0,
                gui_params=GuiParameters(),
                progress_bar=ProgressBarParameters(x=77, y=33,
                                                   texture="arrow"),
                efficiency_bar=RecipeEfficiencyBarParameters(x=38, y=66),
                energy_bar=EnergyBarParameters(x=18, y=30),
                item_slots=ItemSlots(positions=[
                    SlotPosition(x=56, y=36, cols=1, rows=1),
                    SlotPosition(x=102, y=27, cols=1, rows=2)
                ]),
                fluid_slots=FluidSlots(positions=[]),
                front_overlay=True,
                top_overlay=False,
                side_overlay=False,
                tiers=20,
                io_bucket_capacity=16)
]

mi_fixed_machines: Dict[str, MachineData] = {
    it.machine: it
    for it in mi_fixed_machines_list
}
