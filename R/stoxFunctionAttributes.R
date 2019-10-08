# A list of 
stoxFunctionAttributes <- list(
    # Read input biotic data:
    ReadBiotic = list(
        functionCategory = "Baseline", 
        functionOutputDataType = "BioticData", 
        functionParameterHierarchy = list(
            FileNames = list()
        )
    ), 
    # Read strata polygons:
    DefineStrata = list(
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumPolygon", 
        functionParameterHierarchy = list(
            FileName = list(
                UseProcessData = FALSE
            ), 
            UseProcessData = list()
        )
    ), 
    # Calculate areas of strata polygons:
    StratumArea = list(
        functionCategory = "Baseline", 
        functionOutputDataType = "StratumArea", 
        functionParameterHierarchy = list(
            StratumPolygon = list(), 
            AreaMethod = list()
        )
    )
)

