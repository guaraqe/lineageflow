call:
  {
    lineageflow-base = call ../infrastructure/lineageflow-base {};
    lineageflow-types = call ../infrastructure/lineageflow-types {};
    lineageflow-prelude = call ../infrastructure/lineageflow-prelude {};
    lineageflow-declaration = call ../infrastructure/lineageflow-declaration {};
    lineageflow-query = call ../infrastructure/lineageflow-query {};
    lineageflow-io = call ../infrastructure/lineageflow-io {};
    lineageflow-io-cbor = call ../infrastructure/lineageflow-io-cbor {};
    lineageflow-database = call ../infrastructure/lineageflow-database {};
    lineageflow-database-sqlite = call ../infrastructure/lineageflow-database-sqlite {};
    lineageflow-algorithm = call ../infrastructure/lineageflow-algorithm {};

    lineageflow-tracking = call ../algorithms/lineageflow-tracking {};
    lineageflow-derivatives = call ../algorithms/lineageflow-derivatives {};
    lineageflow-homogenization = call ../algorithms/lineageflow-homogenization {};
    lineageflow-deviations = call ../algorithms/lineageflow-deviations {};
    lineageflow-triangulations = call ../algorithms/lineageflow-triangulations {};
    lineageflow-statistics = call ../algorithms/lineageflow-statistics {};
    lineageflow-forces = call ../algorithms/lineageflow-forces {};
    lineageflow-trajectories = call ../algorithms/lineageflow-trajectories {};
    lineageflow-clustering = call ../algorithms/lineageflow-clustering {};

    lineageflow-plot-interface = call ../tools/lineageflow-plot-interface {};
    lineageflow-plot = call ../tools/lineageflow-plot {};
    lineageflow-viewer-interface = call ../tools/lineageflow-viewer-interface {};
    lineageflow-viewer = call ../tools/lineageflow-viewer {};
    lineageflow-server-api = call ../tools/lineageflow-server-api {};
    lineageflow-server = call ../tools/lineageflow-server {};
    lineageflow-script = call ../tools/lineageflow-script {};
    lineageflow-playground = call ../tools/lineageflow-playground {};
    lineageflow-import = call ../tools/lineageflow-import {};
    lineageflow-export = call ../tools/lineageflow-export {};

    bioemergences-clustering = call ../extra/bioemergences-clustering {};
  }

