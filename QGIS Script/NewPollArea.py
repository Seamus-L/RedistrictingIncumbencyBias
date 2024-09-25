from qgis.core import (QgsProcessing,
                       QgsProcessingAlgorithm,
                       QgsProcessingParameterVectorLayer,
                       QgsProcessingParameterFeatureSink,
                       QgsFeatureRequest,
                       QgsProcessingException,
                       QgsFeatureSink,
                       QgsProcessingOutputVectorLayer,
                       QgsVectorLayer,
                       QgsGeometry,
                       QgsFields,
                       QgsFeature,
                       QgsProject,
                       QgsProcessingFeatureSourceDefinition,
                       QgsProcessingOutputLayerDefinition,
                       QgsWkbTypes)
import processing

class NewPollAreaComputerAlgorithm(QgsProcessingAlgorithm):

    INPUT_NEWGEOM = 'INPUT_NEWGEOM'
    INPUT_OLDGEOM = 'INPUT_OLDGEOM'
    CLIPPED_OLDGEOM = 'CLIPPED_OLDGEOM'
    OUTPUT = 'OUTPUT'

    def initAlgorithm(self, config=None):
        self.addParameter(
            QgsProcessingParameterVectorLayer(
                self.INPUT_NEWGEOM,
                'NewGeom Layer',
                types=[QgsProcessing.TypeVectorPolygon]
            )
        )
        self.addParameter(
            QgsProcessingParameterVectorLayer(
                self.INPUT_OLDGEOM,
                'OldGeom Layer',
                types=[QgsProcessing.TypeVectorPolygon]
            )
        )
        self.addParameter(
            QgsProcessingParameterVectorLayer(
                self.CLIPPED_OLDGEOM,
                'ClippedOldGeom Layer',
                types=[QgsProcessing.TypeVectorPolygon]
            )
        )
        self.addParameter(
            QgsProcessingParameterFeatureSink(
                self.OUTPUT,
                'Output Layer'
            )
        )

    def processAlgorithm(self, parameters, context, feedback):
        input_newgeom = self.parameterAsVectorLayer(parameters, self.INPUT_NEWGEOM, context)
        input_oldgeom = self.parameterAsVectorLayer(parameters, self.INPUT_OLDGEOM, context)
        clipped_oldgeom = self.parameterAsVectorLayer(parameters, self.CLIPPED_OLDGEOM, context)

        if input_newgeom.featureCount() == 0:
            raise QgsProcessingException('NewGeom Layer is empty.')

        # Create the output layer definition
        output_sink, output_dest_id = self.parameterAsSink(parameters, self.OUTPUT, context,
                                                           clipped_oldgeom.fields(), clipped_oldgeom.wkbType(), clipped_oldgeom.sourceCrs())

        # Iterate over each feature in the NewGeom layer
        for feature_newgeom in input_newgeom.getFeatures():
            # Find the corresponding feature in the OldGeom layer based on fednum
            expression = f'"fednum" = \'{feature_newgeom["RidingFi_1"]}\''
            request = QgsFeatureRequest().setFilterExpression(expression)
            matching_feature = next(input_oldgeom.getFeatures(request), None)

            if matching_feature is None:
                feedback.pushInfo(f'No matching feature found in OldGeom layer for RidingFi_1 {feature_newgeom["RidingFi_1"]}')
                continue

            # Create a temporary layer to store the subtracted geometries
            temp_layer = QgsVectorLayer("Polygon?crs={}".format(clipped_oldgeom.sourceCrs().authid()), "temp_layer", "memory")
            temp_provider = temp_layer.dataProvider()

            # Process each feature in the ClippedOldGeom layer
            for clipped_feature in clipped_oldgeom.getFeatures():
                clipped_geometry = clipped_feature.geometry()

                # Subtract OldGeom feature from clipped geometry
                subtracted_geometry = clipped_geometry.difference(matching_feature.geometry())

                # Check if the resulting geometry is valid and not empty
                if subtracted_geometry and not subtracted_geometry.isEmpty():
                    # Create a new feature and set its geometry
                    temp_feature = QgsFeature()
                    temp_feature.setGeometry(subtracted_geometry)
                    temp_feature.setFields(clipped_oldgeom.fields())
                    temp_feature.setAttributes(clipped_feature.attributes())

                    # Add the feature to the temporary layer
                    temp_provider.addFeature(temp_feature)

            # Create a temporary layer for the single feature from the NewGeom layer
            temp_newgeom_layer = QgsVectorLayer("Polygon?crs={}".format(input_newgeom.sourceCrs().authid()), "temp_newgeom_layer", "memory")
            temp_newgeom_provider = temp_newgeom_layer.dataProvider()
            temp_newgeom_provider.addFeature(feature_newgeom)

            # Prepare the intersection parameters
            params = {
                'INPUT': temp_layer,
                'OVERLAY': temp_newgeom_layer,
                'OUTPUT': 'memory:'
            }

            # Run intersection of the temporary layer with the single feature from the NewGeom layer
            result = processing.run('native:intersection', params, context=context, feedback=feedback)

            # Add features from the result to the output sink
            for feature in result['OUTPUT'].getFeatures():
                output_sink.addFeature(feature, QgsFeatureSink.FastInsert)

        return {self.OUTPUT: output_dest_id}

    def name(self):
        return 'newpollareacomputer'

    def displayName(self):
        return 'New Poll Area Computer'

    def group(self):
        return 'Example Scripts'

    def groupId(self):
        return 'examplescripts'

    def createInstance(self):
        return NewPollAreaComputerAlgorithm()
