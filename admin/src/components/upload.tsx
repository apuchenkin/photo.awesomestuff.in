import * as React from 'react';
import classNames from 'classnames';
import {
  DropTarget,
  DropTargetSpec,
  ConnectDropTarget,
} from 'react-dnd';
import { NativeTypes } from 'react-dnd-html5-backend';
import { compose, values, indexBy, assoc, path } from 'ramda';
import { ServiceContext } from '@app/context';

interface UploadFile {
  file: File;
  status: Symbol;
  progress?: number;
  error?: string;
}

interface Props {
  files: UploadFile[];
  category: Category;
  dropTarget: ConnectDropTarget;
  hovered: boolean;
  setFiles: (files: UploadFile[]) => void;
  setFile: (file: UploadFile) => void;
  upload: (file: File, category:Category) => Promise<Category>;
}

const fileTarget: DropTargetSpec<Props> = {
  drop: ({ upload, category, setFile, setFiles }, monitor) => {
    const files = monitor.getItem().files.map((file: UploadFile) => ({
      file,
      status: STATUS_PENDING,
    }));

    files.map((file: UploadFile) => {
      upload(file.file, category)
        .then(() => {
          setFile({ ...file, status: STATUS_COMPLETE });
        })
        .catch(error => {
          setFile({ ...file, status: STATUS_ERROR, error });
        });
    });

    setFiles(files);
  },
};

const STATUS_PENDING = Symbol('pending');
const STATUS_COMPLETE = Symbol('complete');
const STATUS_ERROR = Symbol('error');

const getIcon = (status: Symbol) => {
  switch (status) {
    case STATUS_PENDING: return 'pause';
    case STATUS_COMPLETE: return 'check_circle';
    case STATUS_ERROR: return 'error';
    default:
      break;
  }
}

interface FileStatusProps {
  file: UploadFile;
}

const FileStatus: React.FunctionComponent<FileStatusProps> = ({
  file,
}) => (
  <li className="upload-file">
    <span className="title">{file.file.name}</span>
    {file.progress && (
      <span className="progress">
        <span className="bar" style={{ width: `${file.progress}%` }} />
      </span>
    )}
    <span className="status">
      <i className="material-icons" title={file.error} >
        {getIcon(file.status)}
      </i>
    </span>
  </li>
);

const Upload: React.FunctionComponent<Props> = ({ files, dropTarget, children, hovered }) => {
  return dropTarget(
    <div className={classNames('upload', { hovered })}>
      { files.length ? (
        <ul>
          {files.map(file => (
            <FileStatus
              key={file.file.name}
              file={file}
            />
          ))}
        </ul>
      ) : children }
    </div>,
  );
}

export default compose(
  (cmp: React.ComponentType<any>) => (props: any) => {
    const [files, setFiles] = React.useState({});
    const { photoService } = React.useContext(ServiceContext);

    return React.createElement(cmp, {
      ...props,
      files: values(files),
      setFiles: (files: UploadFile[]) => setFiles(indexBy(path(['file', 'name']), files)),
      setFile: (file: UploadFile) => setFiles(files => assoc(file.file.name, file, files)),
      upload: photoService.upload,
    });
  },
  DropTarget(
    NativeTypes.FILE,
    fileTarget,
    (connect, monitor) => ({
      dropTarget: connect.dropTarget(),
      hovered: monitor.isOver() && monitor.canDrop(),
    }),
  ),
)(Upload);

