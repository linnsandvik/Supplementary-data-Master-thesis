import cobra  # why do we have to import in this file, is it not enough to do it in main?
import sys
import csv

import pandas

SOLVER = "glpk"  # endrer fra gurobi fordi jeg ikke har det hehe


def extract_lethal_elements(gene_df):  # this function is used by crypt_find
    lethal_df = gene_df[abs(gene_df["growth"]) <1e-5]
    return [list(gene)[0] for gene in lethal_df.ids.values]


def read_reactions_csv(fn):  # this function is used by crypt_find
    reaction_ids = []
    with open(fn, "r") as f:
        reader = csv.reader(f)
        for row in reader:
            for x in row:
                if len(x.strip()):
                    reaction_ids.append(x.strip())
    return reaction_ids


def crypt_find(model, growth_medium_list, selected_growth_medium,
               element_type="gene", source_type="carbon", solver=None,
               default_carbon_source="EX_flc_e", default_nitrogen_source="EX_nh4_e"):
    if not solver:
        solver = SOLVER

    if isinstance(model, str):
        try:
            model = cobra.io.read_sbml_model(model)
        except IOError:
            raise ValueError("Could not read {0}".format(model))  # change: added ValueError, because with raise
            #  alone we get exception must derive from BaseException error
            #  return False  #unreachable # This line is commented out because it is unreachable?

    if selected_growth_medium in growth_medium_list:
        growth_medium_list.pop(growth_medium_list.index(selected_growth_medium))

    # Set glucose uptake to 0
    if model.optimize().objective_value and (model.optimize().objective_value > 1e-9):  # change: .f to
        # .objective_value (for readability only)
        if source_type == "carbon":
            model.reactions.get_by_id(default_carbon_source).lower_bound = 0
        else:
            model.reactions.get_by_id(default_nitrogen_source).lower_bound = 0

    if model.optimize().objective_value and (model.optimize().objective_value > 1e-9):  # again, .f to .objective_value
        print("""The model is growing after removing the default carbon / nitrogen source,
              please set any uptake of carbon / nitrogen to zero, or change the
              input parameter default_carbon_source / default_nitrogen_source""")
        return False

    # Get initial list of lethal genes
    model.reactions.get_by_id(selected_growth_medium).lower_bound = -10

    if element_type == "gene":
        full_element_dict = cobra.flux_analysis.single_gene_deletion(model, solver=solver, processes=1)  # added "processes =1"

    else:
        full_element_dict = cobra.flux_analysis.single_reaction_deletion(model, solver=solver, processes=1)

    essential_elements = extract_lethal_elements(full_element_dict)
    model.reactions.get_by_id(selected_growth_medium).lower_bound = 0

    for r_id in growth_medium_list:
        model.reactions.get_by_id(r_id).lower_bound = -10
        o = model.optimize()
        if not (o.objective_value or 0) > 1e-9:
            print("Model dead on {0}. Skip this source".format(r_id))
            continue
        if element_type == "gene":
            print(essential_elements)
            element_dict = cobra.flux_analysis.single_gene_deletion(model, essential_elements, solver=solver, processes=1)
        else:
            element_dict = cobra.flux_analysis.single_reaction_deletion(model, essential_elements, solver=solver, processes=1)

        model.reactions.get_by_id(r_id).lower_bound = 0
        new_essential_elements = extract_lethal_elements(element_dict)
        essential_elements = [x for x in essential_elements if x not in new_essential_elements]  # changed from
        #  "if not x in" to "if x not in"
        print("New essential {1}s in {0}: ".format(r_id, element_type), len(new_essential_elements))
        if len(essential_elements) < 10:
            print("Essential {0}s: ".format(element_type), len(essential_elements), essential_elements)
        else:
            print("Essential {0}s: ".format(element_type), len(essential_elements))

    print("Your identfied cryptic gene / reactions")  # why do we print and return?? shouldn't we choose one?
    print(essential_elements)
    return essential_elements
    
