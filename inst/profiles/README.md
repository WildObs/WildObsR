# Vendored Camtrap DP profiles

Used by `as_camtrapdp()` to convert a WildObs data package to canonical Camtrap DP.
Vendored so the conversion runs offline and gives the same answer on every machine.
**Do not edit these files**; rebuild them with the "Camtrap DP profiles" section of
`dev/add_data_to_package.R`, which checks every upstream copy is byte-identical to TDWG's.

| Files | Source |
|---|---|
| `camtrap-dp-profile-<version>.json` | `https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/camtrap-dp-profile.json` |
| `camtrap-dp-<resource>-table-schema-<version>.json` | `https://raw.githubusercontent.com/tdwg/camtrap-dp/<version>/<resource>-table-schema.json` |
| `frictionless-data-package.json` | `https://specs.frictionlessdata.io/schemas/data-package.json` |
| `geojson.json` | `http://json.schemastore.org/geojson.json` |
| `wildobs/` | The WildObs flavour of Camtrap DP (`1.0.2-wildobs.1`), generated in the WildObs camDB repository. See `wildobs/README_wildobs_schemas.md`. |

The TDWG files define what canonical Camtrap DP contains. The WildObs flavour declares
which fields WildObs adds, so `as_camtrapdp()` can label each removal as an expected WildObs
extension or something unexpected.

Versions available: 1.0.1 and 1.0.2. Their table schemas are field-for-field identical;
only the URLs they cite differ.

The Camtrap DP profiles and table schemas are © 2021 Camtrap DP Development Team, MIT
License (<https://github.com/tdwg/camtrap-dp/blob/main/LICENSE>). The frictionless and
GeoJSON schemas are published openly by their maintainers for exactly this kind of reuse.
