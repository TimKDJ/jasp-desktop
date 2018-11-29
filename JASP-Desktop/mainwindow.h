//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

#ifndef MAINWIDGET_H
#define MAINWIDGET_H

#include <QMainWindow>
#include <QSettings>

#include "dataset.h"

#include "datasettablemodel.h"
#include "variablespage/levelstablemodel.h"
#include "variablespage/labelfiltergenerator.h"
#include "enginesync.h"
#include "analyses.h"

#include "analysisforms/analysisform.h"
#include "asyncloader.h"
#include "asyncloaderthread.h"
#include "fileevent.h"
#include "resultsjsinterface.h"
#include "customwebenginepage.h"
#include "columnsmodel.h"
#include "jsonutilities.h"
#include "computedcolumnsmodel.h"

#include "ribbons/ribbonwidget.h"
#include "filtermodel.h"

class ResultsJsInterface;

namespace Ui {
class MainWindow;
}

class MainWindow : public QMainWindow
{
	Q_OBJECT

	friend class ResultsJsInterface;
public:
	explicit MainWindow(QApplication *application);
	void open(QString filepath);
	void testLoadedJaspFile(int timeOut);

	~MainWindow() override;

	EngineSync* _engineSync;

	Q_INVOKABLE void showHelpFromQML(QString pageName);

public slots:
	void setPPIHandler(int ppi, bool refreshAllAnalyses = true);
	void setImageBackgroundHandler(QString value);
	void setUIScaleHandler(float scale);

protected:
	void resizeEvent(QResizeEvent *event)		override;
	void dragEnterEvent(QDragEnterEvent *event) override;
	void dropEvent(QDropEvent *event)			override;
	void closeEvent(QCloseEvent *event)			override;

private:
	void makeConnections();
	void initQWidgetGUIParts();
	void StartOnlineDataManager();


	void packageChanged(DataSetPackage *package);
	void packageDataChanged(DataSetPackage *package, std::vector<std::string> &changedColumns, std::vector<std::string> &missingColumns, std::map<std::string, std::string> &changeNameColumns,	bool rowCountChanged);
	void refreshAnalysesUsingColumns(std::vector<std::string> &changedColumns, std::vector<std::string> &missingColumns, std::map<std::string, std::string> &changeNameColumns, bool rowCountChanged);

	void setDataSetAndPackageInModels(DataSetPackage *package);
	bool closeRequestCheck(bool &isSaving);

	AnalysisForm* loadForm(Analysis *analysis);
	AnalysisForm* loadForm(const std::string name);

	void closeCurrentOptionsWidget();
	void removeAnalysis(Analysis *analysis);

	QString escapeJavascriptString(const QString &str);
	void getAnalysesUserData();
	Json::Value getResultsMeta();

	void startDataEditor(QString path);
	void checkUsedModules();
	void resultsPageLoaded(bool success, int ppi);
	void analysisUnselectedHandler();
	void setPackageModified();
	void analysisSelectedHandler(int id);
	void saveTextToFileHandler(const QString &filename, const QString &data);
	void analysisChangedDownstreamHandler(int id, QString options);
	void analysisSaveImageHandler(int id, QString options);
	void analysisEditImageHandler(int id, QString options);
	void removeAnalysisRequestHandler(int id);
	void matchComputedColumnsToAnalyses();

	void startComparingResults();
	void analysesForComparingDoneAlready();
	void finishComparingResults();

	bool filterShortCut();
	void loadQML();
	void connectRibbonButton(RibbonWidget * ribbon)								{ connect(ribbon,										QOverload<QString>::of(&RibbonWidget::itemSelected),				this,	&MainWindow::itemSelected); }

	void pauseEngines();
	void resumeEngines();

signals:
	void updateAnalysesUserData(QString userData);
	void ppiChanged(int newPPI);
	void imageBackgroundChanged(QString value);

private slots:
	void showForm(Analysis *analysis);

	void analysisResultsChangedHandler(Analysis* analysis);
	void analysisImageSavedHandler(Analysis* analysis);

	void removeAllAnalyses();
	void refreshAllAnalyses();
	void refreshAnalysesUsingColumn(QString col);
	void updateShownVariablesModel();

	void tabChanged(int index);
	void helpToggled(bool on);

	void dataSetIORequest(FileEvent *event);
	void dataSetIOCompleted(FileEvent *event);
	void populateUIfromDataSet();
	void itemSelected(const QString &item);

	void adjustOptionsPanelWidth();
	void splitterMovedHandler(int, int);

	void hideOptionsPanel();
	void showOptionsPanel();
	void showDataPanel();
	void hideDataPanel();
	void startDataEditorHandler();
	void startDataEditorEventCompleted(FileEvent *event);

	void analysisOKed();
	void analysisRunned();

	void updateMenuEnabledDisabledStatus();

	void saveKeysSelected();
	void openKeysSelected();
	void syncKeysSelected();
	void refreshKeysSelected();
	void zoomInKeysSelected();
	void zoomOutKeysSelected();
	void zoomEqualKeysSelected();

	void illegalOptionStateChanged(AnalysisForm * form);
	void fatalError();

	void helpFirstLoaded(bool ok);
	void requestHelpPage(const QString &pageName);

	void emptyValuesChangedHandler();

	void resizeVariablesWindowLabelColumn();
	void closeVariablesPage();

	void showProgress();
	void hideProgress();
	void setProgressStatus(QString status, int progress);

	void updateExcludeKey();
	void dataSetChanged(DataSet * dataSet);
	void unitTestTimeOut();


private:
	typedef std::map<Analysis*, AnalysisForm*> analysisFormMap;

	Ui::MainWindow					*ui;

	Analyses						*_analyses;
	ResultsJsInterface				*_resultsJsInterface;
	AnalysisForm					*_currentOptionsWidget	= NULL;
	DataSetPackage					*_package;
	DataSetTableModel				*_tableModel			= NULL;
	LevelsTableModel				*_levelsTableModel;
	Analysis						*_currentAnalysis		= NULL;
	labelFilterGenerator			*_labelFilterGenerator;
	ColumnsModel					*_columnsModel			= NULL;
	ComputedColumnsModel			*_computedColumnsModel	= NULL;
	FilterModel						*_filterModel			= NULL;
	OnlineDataManager				*_odm;
	
	analysisFormMap					_analysisFormsMap;
	TableModelVariablesAvailable	_availableVariablesModel;

	int								_scrollbarWidth = 0,
									_tableViewWidthBeforeOptionsMadeVisible;


	QString							_lastRequestedHelpPage,
									_openOnLoadFilename,
									_fatalError,
									_currentFilePath;

	AsyncLoader						_loader;
	AsyncLoaderThread				_loaderThread;
	QObject							*qmlProgressBar				= NULL;

	bool							_inited,
									_applicationExiting		= false,
									_resultsViewLoaded		= false,
									_openedUsingArgs		= false,
									_excludeKey				= false;

	QWidget							*_buttonPanel;
	QVBoxLayout						*_buttonPanelLayout;
	QPushButton						*_okButton,
									*_runButton;

	QSettings						_settings;
	CustomWebEnginePage				*_customPage;
	QApplication					*_application = nullptr;

	void							_analysisSaveImageHandler(Analysis* analysis, QString options);
};

#endif // MAINWIDGET_H
