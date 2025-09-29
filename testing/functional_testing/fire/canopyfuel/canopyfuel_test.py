"""
Concrete class for running the canopyfuel functional test for FATES.
"""
import os
import numpy as np
import xarray as xr
import pandas as pd
import matplotlib.pyplot as plt

from functional_class import FunctionalTest
from utils_plotting import blank_plot

COLORS = ["#793922", "#99291F", "#CC9728", "#6B8939", "#2C778A", "#2C378A"]


class CanopyFuelTest(FunctionalTest):
    """Canopy fuel test class"""

    name = "canopyfuel"

    def __init__(self, test_dict):
        super().__init__(
            CanopyFuelTest.name,
            test_dict["test_dir"],
            test_dict["test_exe"],
            test_dict["out_file"],
            test_dict["use_param_file"],
            test_dict["other_args"],
        )
        self.plot = True

    def plot_output(self, run_dir: str, save_figs: bool, plot_dir: str):
        """Plot output associated with canopy fuel tests

        Args:
            run_dir (str): run directory
            out_file (str): output file
            save_figs (bool): whether or not to save the figures
            plot_dir (str): plot directory
        """

        # read in canopy fuel data
        cfuel_dat = xr.open_dataset(os.path.join(run_dir, self.out_file))

        self.plot_barchart(
            cfuel_dat,
            "CBD",
            "Canopy bulk density",
            "kg m$^{-3}$",
            save_figs,
            plot_dir,
            by_fuel_model=False,
        )
        self.plot_barchart(
            cfuel_dat,
            "CBH",
            "Canopy base height",
            "m",
            save_figs,
            plot_dir,
            by_fuel_model=False,
        )
        self.plot_barchart(
            cfuel_dat,
            "ROS_active",
            "Active crown fire ROS",
            "m min$^{-1}$",
            save_figs,
            plot_dir,
            by_fuel_model=False,
        )
        self.plot_barchart(
            cfuel_dat,
            "ROS_min",
            "Critical ROS",
            "m min$^{-1}$",
            save_figs,
            plot_dir,
            by_fuel_model=False,
        )

    @staticmethod
    def plot_barchart(
        cfuel_dat: xr.Dataset,
        var: str,
        varname: str,
        units: str,
        save_figs: bool,
        plot_dir: bool,
        by_fuel_model: bool = False,

    ):
        """ Plot canopy fuel test outputs as bar plot
        Args:
        cfuel_dat (xr.Dataset): canopy fuel test data output
        var (str): variable to plot
        varname (str): variable name for y-axis
        units (str): unit expression
        save_figs (bool): whether to save figure or not
        plot_dir (bool): which dir to save figs
        by_fuel_model (bool, optional): whether or not the bar plot is grouped by fuel model, default to True

        """
        fuel_models = [
            "FM10",
        ]

        colors = [
            "black",
        ]

        patch_types = [str(f) for f in cfuel_dat.patch_type.values]

        if by_fuel_model:
            data_dict = {
                fm: cfuel_dat.isel(fuel_model=i)[var].values
                for i, fm in enumerate(fuel_models)
            }
        else:
            data_dict = cfuel_dat[var].values

        _, ax = plt.subplots()
        if by_fuel_model:
            bottom = np.zeros(len(patch_types))
            for i, (fuel_model, dat) in enumerate(data_dict.items()):
                ax.bar(
                    patch_types,
                    dat,
                    0.5,
                    label=fuel_model,
                    bottom=bottom,
                    color=colors[i],
                )
                bottom += dat
            plt.legend(loc="center left", bbox_to_anchor=(1, 0.5))
        else:
            ax.bar(patch_types, data_dict, color="darkcyan")

        box = ax.get_position()
        ax.set_position([box.x0, box.y0, box.width * 0.75, box.height])
        plt.ylabel(f"{varname} ({units})", fontsize=11)
        plt.xticks(rotation=90)
        plt.xlabel("Patch Type")

        if save_figs:
            fig_name = os.path.join(plot_dir, f"{varname}_plot.png")
            plt.savefig(fig_name)
        



  