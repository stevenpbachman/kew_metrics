<?xml version="1.0" encoding="UTF-8"?>
<ArcPad>
	<LAYER name="TIPAs_network_boundaries_OUTLINES" readonly="true">
		<SYMBOLOGY>
			<SIMPLELABELRENDERER visible="false" field="TIPA" refscale="1:219452.817382" rotationfield="" expression="" language="">
				<TEXTSYMBOL fontcolor="Black" font="Arial" fontsize="8" horzalignment="center" vertalignment="center" rtl="false" fontstyle="regular">
				</TEXTSYMBOL>
			</SIMPLELABELRENDERER>
			<SIMPLERENDERER>
				<SIMPLEPOLYGONSYMBOL filltype="solid" fillcolor="Null" filltransparency="0" backgroundcolor="Black" boundarycolor="255,0,127" boundarywidth="4"/>
			</SIMPLERENDERER>
		</SYMBOLOGY>
		<FORMS>
			<EDITFORM name="TIPAs_network_boundaries_OUTLINES" caption="TIPAs_network_boundaries_OUTLINES" width="130" height="130" attributespagevisible="false">
				<PAGE name="page1" caption="Page 1">
					<LABEL name="lblNAME" x="2" y="3" width="60" height="12" tabstop="false" caption="NAME">
					</LABEL>
					<EDIT name="txtNAME" x="64" y="2" width="60" height="12" tabstop="true" required="true" readonly="true" field="NAME">
					</EDIT>
					<LABEL name="lblTIPA" x="2" y="18" width="60" height="12" tabstop="false" caption="TIPA">
					</LABEL>
					<EDIT name="txtTIPA" x="64" y="17" width="60" height="12" tabstop="true" required="true" readonly="true" field="TIPA">
					</EDIT>
					<LABEL name="lblBoundary" x="2" y="33" width="60" height="12" tabstop="false" caption="Boundary">
					</LABEL>
					<EDIT name="txtBoundary" x="64" y="32" width="60" height="12" tabstop="true" required="true" readonly="true" field="Boundary">
					</EDIT>
				</PAGE>
			</EDITFORM>
			<QUERYFORM name="QUERYFORM" caption="Query Form" width="130" height="130">
				<PAGE name="page1" caption="TIPAs_network_boundaries_OUTLINES Query (Page 1)">
					<LABEL name="lblNAME" x="2" y="3" width="60" height="12" tabstop="false" caption="NAME">
					</LABEL>
					<EDIT name="txtNAME" x="64" y="2" width="60" height="12" tabstop="true" required="true" readonly="false" field="NAME">
					</EDIT>
					<LABEL name="lblTIPA" x="2" y="18" width="60" height="12" tabstop="false" caption="TIPA">
					</LABEL>
					<EDIT name="txtTIPA" x="64" y="17" width="60" height="12" tabstop="true" required="true" readonly="false" field="TIPA">
					</EDIT>
					<LABEL name="lblBoundary" x="2" y="33" width="60" height="12" tabstop="false" caption="Boundary">
					</LABEL>
					<EDIT name="txtBoundary" x="64" y="32" width="60" height="12" tabstop="true" required="true" readonly="false" field="Boundary">
					</EDIT>
				</PAGE>
			</QUERYFORM>
		</FORMS>
		<METADATA/>
		<QUERY where=""/>
	</LAYER>
</ArcPad>
