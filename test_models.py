import pathlib

from flamapy.metamodels.fm_metamodel.transformations import UVLReader
from flamapy.metamodels.z3_metamodel.transformations import FmToZ3
from flamapy.metamodels.z3_metamodel.operations import Z3Satisfiable


UVL_MODELS_DIR = 'models'


def get_filepaths(directory: str, extensions_filter: list[str] = None) -> list[str]:
    """
    Obtains all filepaths of files with the given extensions from the specified 
    directory and its subdirectories.

    :param directory: The root directory as a string or Path object.
    :param extensions_filter: Optional list of file extensions (e.g., ['.uvl', '.json']).
    :return: List of filepaths as strings.
    """
    root_path = pathlib.Path(directory)
    filepaths = []
    
    if extensions_filter is None:
        extensions_filter = []
    
    norm_filters = [ext.lower() for ext in extensions_filter]
    for path in root_path.rglob('*'):
        if path.is_file():
            if not norm_filters or any(path.name.lower().endswith(ext) for ext in norm_filters):
                filepaths.append(str(path))
    return filepaths


def main() -> None:
    models = get_filepaths(UVL_MODELS_DIR, ['.uvl'])
    total_models = len(models)
    models_with_errors = []
    models_ok = 0
    models_errors = 0
    sat_count = 0
    unsat_count = 0
    for i, model in enumerate(models, 1):
        try:
            print(f'🚀 ({i} / {total_models}) {model}')
            fm_model = UVLReader(model).transform()
            print(f'  ✅ ({i} / {total_models}) {model}')
            models_ok += 1
            z3_model = FmToZ3(fm_model).transform()
            satisfiable = Z3Satisfiable().execute(z3_model).get_result()
            print(f'    🔎 Satisfiable: {satisfiable}')
            if satisfiable:
                sat_count += 1
            else:
                unsat_count += 1
        except Exception as e:
            if 'faulty' in model:
                print(f'  ✅ ({i} / {total_models}) {model}')
            else:
                print(f'  ❌ ({i} / {total_models}) {model}')
                models_errors += 1
    print(f'\nSummary: ')
    print(f'Total models: {total_models}')
    print(f'Models OK ✅: {models_ok} ({(models_ok/total_models)*100:.2f}%)')
    print(f'Models with errors ❌: {models_errors} ({(models_errors/total_models)*100:.2f}%)')
    print(f'Satisfiable models: {sat_count} ({(sat_count/total_models)*100:.2f}%)')
    print(f'Unsatisfiable models: {unsat_count} ({(unsat_count/total_models)*100:.2f}%)')

if __name__ == '__main__':
    #model = UVLReader('test_resources/parsing/type_level/fm06_string_bounded.uvl').transform()
    #print(model)
    #raise Exception
    main()