# NetflixData-Shiny-app 

<div style="display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 10px;">
  <img src="https://github.com/user-attachments/assets/3f938dc4-7650-44c5-8858-b49f66b2249c" height="200px" width="300px">
  <img src="https://github.com/user-attachments/assets/54890027-d6de-4b24-98ac-b453c56881e8" height="200px" width="300px">
  <img src="https://github.com/user-attachments/assets/4c6aa405-4c71-4237-bad4-84e2a36d9b3b" height="200px" width="300px">
  <img src="https://github.com/user-attachments/assets/b2a83550-d373-4748-b396-d49ecfcf4b39" height="200px" width="300px">
  <img src="https://github.com/user-attachments/assets/66fe20e3-1f4f-4f10-860d-11b6d78baccc" height="200px" width="300px">
  <img src="https://github.com/user-attachments/assets/551c0a43-78eb-4cd8-8d35-e3e32de32261" height="200px" width="300px">
</div>

此仓库包含 Netflix 标题数据集以及用于分析数据的 `R` 脚本。分析涵盖了 Netflix 数据集的各种方面，例如类型、制作国家和上映日期。脚本使用 R 进行数据操作和可视化。

## 功能与特点

1. **下载功能**:  
   - 允许用户下载数据文件或应用生成的特定报告。
   - 提供 CSV 或 PDF 格式的下载选项，方便查看自定义数据集。

2. **排序功能**:  
   - 用户可以根据不同的标准对表格或可视化进行排序（例如国家、数值、日期等）。
   - 排序可以是升序或降序。

3. **页面导航**:  
   - 提供标签或按钮用于在应用的不同部分之间导航（例如概览、详细分析和报告）。
   - 使用 Shiny 的反应式元素或 WebGL 过渡，确保页面切换顺畅。

4. **带有点击跳转的交互式柱状图**:  
   - 柱状图包含与各个国家相关的数据。
   - 用户可以点击柱状图中的数字，跳转到特定国家的详细数据视图，或对该国家的内容进行筛选。

5. **筛选功能**:  
   - 用户可以根据地区、年份或数据范围等类别应用筛选器。
   - 这些筛选器会动态更新图表和表格。

6. **动态数据可视化**:  
   - 当用户与应用交互时，实时更新可视化（例如根据筛选条件或输入变化更新图表）。

7. **悬停提示**:  
   - 将鼠标悬停在数据点、柱状图或图表元素上时，会显示该特定元素的详细信息或统计数据。
   
8. **数据导出**:  
   - 可以导出图表和表格，允许用户以图像形式下载可视化或保存报告。
   
9. **搜索功能**:  
   - 搜索栏允许用户搜索特定国家、数据点或数据集内的部分。

10. **国家特定详细信息**:  
    - 在图表中选择或点击某个特定国家后，会显示该国家的详细数据，包括人口、GDP 或其他相关指标。

## 项目结构

- **app.R**: 包含用于分析 Netflix 数据集代码的 R 脚本。
- **netflix_titles.csv**: 用于分析的数据集，包含 Netflix 电视节目和电影的相关信息。

## 数据集

`netflix_titles.csv` 数据集包括以下字段：
- `show_id`: 每个节目的唯一 ID。
- `type`: 内容类型（电影或电视剧）。
- `title`: 内容的标题。
- `director`: 内容的导演（如果有的话）。
- `cast`: 内容的演员阵容。
- `country`: 内容的制作国家。
- `date_added`: 内容被添加到 Netflix 的日期。
- `release_year`: 内容的上映年份。
- `rating`: 内容的分级（例如 PG、R）。
- `duration`: 内容的时长（电影为分钟，电视剧为季数）。
- `listed_in`: 所属类型。
- `description`: 内容的简短描述。

## 如何运行代码

1. 克隆仓库：
    ```bash
    git clone https://github.com/your-username/netflix-titles-analysis.git
    ```

2. 确保您已安装 R 以及所需的依赖包（例如 `tidyverse`, `ggplot2`）。

3. 在 R 环境中运行 `app.R` 文件以执行分析。

## 分析重点

本次分析主要集中于：
- **内容分布**: 分析电视节目与电影的数量。
- **按国家制作分析**: 探索制作最多内容的国家。
- **类型探索**: 可视化不同类型内容的分布情况。
- **上映年份趋势**: 研究各年份内容的发布趋势。

## 分析结果

您可以在 `app.R` 脚本中找到详细的见解和可视化结果。主要分析结论包括：
- Netflix 内容发布量逐年增加。
- 特定类型如纪录片、喜剧和剧情片占据主导地位。
- 美国是 Netflix 内容的主要生产国。

## 未来改进

- 包含更详细的内容评级分析。
- 开发基于类型或其他因素的推荐系统。
- 探索全球与地区性内容分布的更多见解。

## 许可证

本项目基于 Apache 许可证进行许可 - 详见 LICENSE 文件。

