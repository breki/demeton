- define some top-level integration test cases and ignore + todo next them

Bounds -> tile ids -> tile file names -> height array -> image stream

- split up the logic into independent pieces:
	1. fetching the DEM data
	2. generating hillshading model
	3. writing the model into PNG stream

- find tile files (.hgt) in local cache
- if not found in the local storage, find zips on the main store
- unzip file