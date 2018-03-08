import subprocess
import json
import tempfile
import glob
import argparse
import uuid

arg_parser = argparse.ArgumentParser(description='SQS Message Wrangler Tool - YEAH!')
arg_parser.add_argument(
	'-r', '--read-queue-url',
	help='The URL of the AWS SQS queue you wish READ from',
	default='<none>')
arg_parser.add_argument(
	'-w', '--write-queue-url',
	help='The URL of the AWS SQS queue you wish WRITE to',
	default='<none>')
arg_parser.add_argument(
	'-t', '--timeout',
	help='The number of seconds to wait for messages before giving up',
	type=int,
	default=5)
arg_parser.add_argument(
	'-d', '--temp-directory',
	help='The directory used to cache messages as they are READ from the SQS queue',
	default='<temp>')
arg_parser.add_argument(
	'--sns-mode',
	help='Behavior switch which will cause the script to parse each message as an SNS-wrapped message.',
	action='store_true')
arg_parser.add_argument(
	'--remove-timestamps-from-message',
	help='Behavior switch which will cause the script to parse and remove the \'timestamp\' field from the message body before WRITING.',
	action='store_true')
arg_parser.add_argument(
	'--debug',
	help='Print DEBUG messages.',
	action='store_true')
args = arg_parser.parse_args()

def error(message):
	print('[ERROR] ' + message)
def info(message):
	print('[INFO] ' + message)
def debug(message):
	if args.debug:
		print('[DEBUG] ' + message)

try:
	aws_bin = subprocess.check_output(['which', 'aws']).decode('ascii').strip()
except subprocess.CalledProcessError:
	error('You must install the AWS CLI tools. To do so, please execute the following command and then try again:\n\n\tsudo pip install awscli --ignore-installed six\n\nAfter doing so, you must configure the AWS CLI using the following command:\n\n\taws configure\n')
	raise SystemExit(0)
info('Found AWS CLI tools at ' + aws_bin)

if args.temp_directory == '<temp>':
	temp_dir = tempfile.mkdtemp(prefix='sqs-wrangler-')
else:
	temp_dir = args.temp_directory
info('Using temporary directory \'' + temp_dir + '\'')

if args.sns_mode:
	debug('~~~ SNS mode is ACTIVE ~~~')

message_files = None

if args.read_queue_url != '<none>':
	info('Beginning READ from SQS queue \'' + args.read_queue_url + '\'')
	message_files = []
	while True:
		try:
			request = {
				'QueueUrl': args.read_queue_url,
				'MaxNumberOfMessages': 10,
				'VisibilityTimeout': 10,
				'WaitTimeSeconds': args.timeout
			}
			reponse = subprocess.check_output(['aws', 'sqs', 'receive-message', '--cli-input-json', json.dumps(request)]).decode('utf-8')
		except subprocess.CalledProcessError as e:
			error('Could not READ messages: ' + str(e))
			raise SystemExit(1)

		if reponse == '':
			break

		messages = json.loads(reponse)
		for message in messages['Messages']:
			message_id = message['MessageId']
			message_file = temp_dir + '/' + message_id + '.json'
			with open(message_file, 'w') as f:
				json.dump(message, f)
			message_files.append(message_file)

		try:
			request = {
				'QueueUrl': args.read_queue_url,
				'Entries': [{'Id': m['MessageId'], 'ReceiptHandle': m['ReceiptHandle']} for m in messages['Messages']]
			}
			response = subprocess.check_output(['aws', 'sqs', 'delete-message-batch', '--cli-input-json', json.dumps(request)]).decode('utf-8')
		except subprocess.CalledProcessError as e:
			error('Could not DELETE messages that were READ: ' + str(e))
			raise SystemExit(1)

		debug('READ and saved ' + str(len(messages['Messages'])) + ' messages; deleting from SQS...')

		deleted = json.loads(response)
		if 'Failed' in deleted:
			error('Failed to DELETE one or more READ messages: ' + str(deleted['Failed']))
			raise SystemExit(1)

	info('Finished READ of SQS; received ' + str(len(message_files)) + ' messages')

if args.write_queue_url != '<none>':
	def update_message_body(body, new_data):
		body_data = json.loads(body)
		data = body_data
		if args.sns_mode:
			data = json.loads(data['Message'])
		debug('Replacing data in ' + str(data) + ' with ' + str(new_data) + '...')
		for k in new_data:
			data[k] = new_data[k]
		if args.sns_mode:
			body_data['Message'] = json.dumps(data)
		return json.dumps(body_data)

	if message_files is None:
		message_files = glob.glob(temp_dir + '/*.json')
	
	if len(message_files) == 0:
		info('Nothing to WRITE...')
		raise SystemExit(0)

	info('Beginning WRITE to SQS \'' + args.write_queue_url + '\'')
	partitioned_message_files = [message_files[i:i + 10] for i in range(0, len(message_files), 10)]
	for files in partitioned_message_files:
		messages = []
		for message_file in files:
			with open(message_file, 'r') as f:
				message = json.load(f)

			body = message['Body']
			if args.remove_timestamps_from_message:
				body = update_message_body(body, {'timestamp': None})

			messages.append({'Id': str(uuid.uuid4()), 'MessageBody': body})

		debug('attempting to WRITE ' + str(len(messages)) + ' messages...')

		try:
			request = {
				'QueueUrl': args.write_queue_url,
				'Entries': messages
			}
			response = subprocess.check_output(['aws', 'sqs', 'send-message-batch', '--cli-input-json', json.dumps(request)]).decode('utf-8')
		except subprocess.CalledProcessError as e:
			error('Could not WRITE messages: ' + str(e))
			raise SystemExit(1)

		written = json.loads(response)
		if 'Failed' in written:
			error('Failed to WRITE one or more messages: ' + str(written['Failed']))
			raise SystemExit(1)

	info('Finished WRITE of ' + str(len(message_files)) + ' messages')

info('Done!')
